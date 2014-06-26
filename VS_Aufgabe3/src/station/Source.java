package station;

import manager.MessageManager;

import java.io.BufferedReader;
import java.io.IOException;

/**
 * Std-Out Daten lesen
 */
public class Source extends Thread {

    private final static Integer dataLength = 24;
    private BufferedReader stdin;
    private Boolean execution;

    /**
     * Konstruktor
     */
    public Source(BufferedReader stdin){
        this.stdin = stdin;
    }

    /**
     * Thread starten
     */
    @Override
    public void run(){
        byte[] data;
        char[] cbuf;
        Message message;
        BufferedReader stdin;
        MessageManager messageManager;

        messageManager = MessageManager.getInstance();
        cbuf = new char[Source.dataLength];
        this.execution = true;

        while(this.execution){
            try {
                this.stdin.read(cbuf, 0, Source.dataLength);

                if(cbuf.length == Source.dataLength){
                    data = new String(cbuf).getBytes();
                    message = new Message();
                    message.write(1, data);
                    messageManager.setNextMessage(message);
                }

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
