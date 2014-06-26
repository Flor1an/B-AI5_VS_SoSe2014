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

        // Berechnung für besseren Einstieg
        try { Thread.sleep(Station.frameDuration / 100 * 95);}
        catch (InterruptedException e) { e.printStackTrace(); }

        /* Random Slot */
        nextSlot = slotManager.getFreeSlot();

        /* Wait for Datenquelle */
        while (messageManager.getNextMessage() == null) {
            try { Thread.sleep(100);}
            catch (InterruptedException e) { e.printStackTrace(); }
        }

        this.execution = true;
        while (this.execution) {

            // Synchronen Versatz berechnen
            clockManager.resetClockToSync();

            // Slots freigeben
            slotManager.resetSlots();

            // Framestart setzen
            clockManager.setframeStart(clockManager.now());
            clockManager.accCount = 1;
            clockManager.accTime = clockManager.getSyncOffset();

            // Slot Verzögerung berechnen
            slotDelay = (long) (((nextSlot - 1) * Station.slotDuration) + (Station.slotDuration / 2));

            // Warten bis Slotfenster erreicht
            try { Thread.sleep(slotDelay);}
            catch (InterruptedException e) { e.printStackTrace(); }

            // Nachricht erstellen
            nextSlot = slotManager.getFreeSlot();
            nextMessage = messageManager.getNextMessage();
            nextMessage.setType(this.type);
            nextMessage.setSlot(nextSlot);
            clockManager.setSyncOffset(Math.round(clockManager.accTime / (double)clockManager.accCount));
            nextMessage.setDate(new Date(clockManager.now()));
            this.sender.send(nextMessage);

            //*

            System.out.println(
                    "SYS " + new Date(System.currentTimeMillis()) +
                    " | NOW " + new Date(clockManager.now()) +
                    " | OFF " + clockManager.getSyncOffset()
            );

            //*/
            

            // Warten bis neuer Frame beginnt
            remaining = Station.frameDuration - (System.currentTimeMillis() % Station.frameDuration);
            try { Thread.sleep(remaining); }
            catch (InterruptedException e) {e.printStackTrace(); }

            // Log
            //System.out.println("\t~~" + (slotDelay + remaining));
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