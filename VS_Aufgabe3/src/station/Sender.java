package station;

import java.io.IOException;
import java.net.*;

/**
 * Muzlticast Nachrichten senden
 */
public class Sender {

    private InetAddress host;
    private Integer port;
    private NetworkInterface face;
    private MulticastSocket multicastSocket;

    /**
     * Konstruktor
     */
    public Sender(String face, String host, Integer port){
        try {
            this.face = NetworkInterface.getByName(face);
            this.host = InetAddress.getByName(host);
            this.port = port;
        } catch (UnknownHostException e) {
            e.printStackTrace();
        } catch (SocketException e) {
            e.printStackTrace();
        }

        try {
            this.multicastSocket = new MulticastSocket(this.port);
            this.multicastSocket.joinGroup(this.host);
            this.multicastSocket.setNetworkInterface(this.face);
            this.multicastSocket.setTimeToLive(1);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * @param message Zu sendene Nachricht
     */
    public void send(Message message){

        byte[] buffer = message.getBytes();
        DatagramPacket datagramPacket = new DatagramPacket(buffer, buffer.length, this.host, this.port);

        try { this.multicastSocket.send(datagramPacket); }
        catch (IOException e) { e.printStackTrace(); }
    }
}
