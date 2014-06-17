package sender;
import java.io.*;
import java.net.*;
import java.util.Date;
/**
 * @author lycog
 */
public class MulticastSender {
  public static void main(String[] args) {
    DatagramSocket socket = null;
    DatagramPacket outPacket = null;
    byte[] outBuf;
    final int PORT = 15003;
 
    try {
      socket = new DatagramSocket();
      long counter = 0;
      String msg;
 
      while (true) {
        msg ="A"+ System.currentTimeMillis() + "team 02-10 " + counter;
        counter++;
        outBuf = msg.getBytes();
 
        //Send to multicast IP address and port
        InetAddress address = InetAddress.getByName("225.10.1.2");
        outPacket = new DatagramPacket(outBuf, outBuf.length, address, PORT);
 
        socket.send(outPacket);
 
        
        System.out.println("Server sends : " + msg);
        try {
          Thread.sleep(100);
        } catch (InterruptedException ie) {
        }
      }
    } catch (IOException ioe) {
      System.out.println(ioe);
    }
  }
}