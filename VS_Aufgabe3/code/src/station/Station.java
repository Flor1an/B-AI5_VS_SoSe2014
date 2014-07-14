package station;


import manager.ClockManager;
import manager.MessageManager;
import manager.SlotManager;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Date;

/**
 * Station
 */
public class Station extends Thread {

    private final static Integer frameDuration = 1000;
    private final static Integer slotDuration = 40;

    private String type;
    private Sender sender;
    private Source source;
    private Receiver receiver;
    private Boolean execution;

    /**
     * Konstruktor
     *
     * @param iface Interface
     * @param host  Host
     * @param port  Port
     * @param type  Type
     */
    public Station(String iface, String host, Integer port, String type, Long utcOffset) {

        System.out.println("Team3 Station start");

        this.type = type;

        this.source = new Source(new BufferedReader(new InputStreamReader(System.in)));

        this.sender = new Sender(iface, host, port);

        this.receiver = new Receiver(iface, host, port);

        ClockManager.getInstance().setSyncOffset(utcOffset);
    }

    /**
     * Thread Datenquelle und Empfänger starten
     */
    public void execute() {
        this.source.start();
        this.receiver.start();
    }

    /**
     * Thread starten
     */
    @Override
    public void run() {
        /* Deklaration */
        Integer nextSlot;
        Long slotDelay, remaining;
        SlotManager slotManager;
        MessageManager messageManager;
        ClockManager clockManager;
        Message nextMessage;

        /* Manager */
        clockManager = ClockManager.getInstance();
        slotManager = SlotManager.getInstance();
        messageManager = MessageManager.getInstance();

        /* Wait for Datenquelle */
        while (messageManager.getNextMessage() == null) {
            try { Thread.sleep(100);}
            catch (InterruptedException e) { e.printStackTrace(); }
        }

        // Berechnung für besseren Einstieg
        try {
            Thread.sleep(clockManager.now() % Station.frameDuration);
            Thread.sleep(Station.frameDuration / 100 * 95);
        }catch (InterruptedException e) { e.printStackTrace(); }

        /* Random Slot */
        while((nextSlot = slotManager.getFreeSlot()) == null){
            System.out.println("Kein freier Slot (Initial) " + (new Date()));
            try { Thread.sleep(500); }
            catch (InterruptedException e){ e.printStackTrace(); }
        }

        /* Loop */
        this.execution = true;
        while (this.execution) {

            // Warten bis neuer Frame beginnt
            try { Thread.sleep(Station.frameDuration - (System.currentTimeMillis() % Station.frameDuration)); }
            catch (InterruptedException e) {e.printStackTrace(); }

            // Berechne neue Zeit
            clockManager.setSyncOffset((long) Math.floor(clockManager.accTime / clockManager.accCount));

            // Framestart setzen
            clockManager.setframeStart(clockManager.now());
            clockManager.accCount = 1;
            clockManager.accTime = clockManager.getSyncOffset();
            Receiver.lastSlot = 0;

            // Slots freigeben
            slotManager.resetSlots();

            // Slot Verzögerung berechnen
            slotDelay = (long) Math.round((nextSlot * Station.slotDuration) - (Station.slotDuration / 2));

            // Warten bis Slotfenster erreicht
            try { Thread.sleep(Math.max((slotDelay + clockManager.getSyncOffset()) % Station.frameDuration, 0));}
            catch (InterruptedException e) { e.printStackTrace(); }

            // Nachricht erstellen
            if((nextSlot = slotManager.getFreeSlot()) != null){
                nextMessage = messageManager.getNextMessage();
                nextMessage.setType(this.type);
                nextMessage.setSlot(nextSlot);
                nextMessage.setDate(new Date(clockManager.now()));
                this.sender.send(nextMessage, nextSlot);
            }else{
                while((nextSlot = slotManager.getFreeSlot()) == null){
                    System.out.println("Kein freier Slot (runtime) " + (new Date()));
                    try { Thread.sleep(500); }
                    catch (InterruptedException e){ e.printStackTrace(); }
                }
            }

            /*
            System.out.println(
                    "" + nextMessage.getName() +
                    " | SYS " + new Date(System.currentTimeMillis()) +
                    " | NOW " + new Date(clockManager.now()) +
                    " | OFF " + clockManager.getSyncOffset()
            );
            */
        }
    }

    /**
     * Thread beenden
     */
    public void terminate() {
        this.execution = false;
    }

    /**
     * Eintrittsmethode
     */
    public static void main(String[] args) {
        if (args.length == 5) {
            String iface = args[0],
                    host = args[1],
                    type = args[3];

            Integer port = Integer.parseInt(args[2]);

            Long offset = Long.parseLong(args[4]);

            Station station = new Station(iface, host, port, type, offset);
            station.start();
            station.execute();
        } else {
            System.out.println("Parameter: <interfaceName> <mcastAddress> <receivePort> <stationClass> <utcOffset>");
        }
    }


}