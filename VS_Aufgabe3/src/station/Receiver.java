package station;

import manager.ClockManager;
import manager.SlotManager;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.NetworkInterface;

/**
 * Multicast Nachrichten lesen
 */
public class Receiver extends Thread {

    private final static Integer messageLength = 34;
    private final static Integer slotDuration = 40;

    private MulticastSocket multicastSocket;

    private Boolean execution;

    /**
     * Konstruktor
     */
    public Receiver(String face, String host, Integer port){

        try {
        	this.multicastSocket = new MulticastSocket(port);
            this.multicastSocket.setNetworkInterface(NetworkInterface.getByName(face));
            this.multicastSocket.joinGroup(InetAddress.getByName(host));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Thread starten
     */
    @Override
    public void run() {
        Message message;
        
        SlotManager slotManager;
        ClockManager clockManager;

        clockManager = ClockManager.getInstance();
        slotManager = SlotManager.getInstance();
        
        this.execution = true;

        Integer lastSlot = 1;
        while (this.execution){
            try {
                byte[] buffer = new byte[messageLength];
                DatagramPacket datagramPacket = new DatagramPacket(buffer, buffer.length);
                this.multicastSocket.receive(datagramPacket);

                Long elapsedTime = clockManager.now() - clockManager.getFrameStart();
                Integer currentSlot = (int)(Math.floor(elapsedTime / slotDuration) + 1);

                if(lastSlot != currentSlot){
                    message = new Message();
                    message.write(0, buffer);
                    
                    Long receiveTime = clockManager.now();

                    if(message.getType().equals("A")){
                        clockManager.accTime += message.getDate().getTime() - receiveTime;
                        clockManager.accCount += 1;
                    }

                    slotManager.lockSlot(message.getSlot());

                }

                lastSlot = currentSlot;

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Thread beenden
     */
    public void terminate() {
        this.execution = false;
    }
}
