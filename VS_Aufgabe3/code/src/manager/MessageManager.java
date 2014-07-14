package manager;

import station.Message;

/**
 * Klasse zur Verwaltung der Nachrichten (Singleton)
 * */
public class MessageManager {

    private static MessageManager instance = new MessageManager();

    private Message nextMessage;

    /**
     * Konstruktor
     * */
    private MessageManager(){
        this.nextMessage = null;
    }

    /**
     * Aktuelle (einzige) Instanz bekommen
     * */
    public static MessageManager getInstance(){
        return MessageManager.instance;
    }

    /**
     * @return Nächste zu sendende Nachricht
     * */
    public Message getNextMessage(){
        return this.nextMessage;
    }

    /**
     * @param message Zu sendende Nachricht
     * */
    public void setNextMessage(Message message){
        this.nextMessage = message;
    }
}
