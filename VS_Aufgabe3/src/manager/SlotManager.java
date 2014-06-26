package manager;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

/**
 * Klasse zur Verwaltung der Nachrichten (Singleton)
 * */
public class SlotManager {

    private final static Integer slotSize = 25;

    private Boolean[] slotArray = new Boolean[slotSize];
    private Set<Integer> slotSet = new HashSet<Integer>();

    private static SlotManager instance = new SlotManager();

    /**
     * Konstruktor
     * */
    private SlotManager(){
        this.resetSlots();
    }

    /**
     * Aktuelle (einzige) Instanz bekommen
     * */
    public static SlotManager getInstance(){
        return SlotManager.instance;
    }

    /**
     * @return Freien Slot berechnen (random)
     * */
    public Integer getFreeSlot(){
        Set<Integer> set = new HashSet<Integer>();

        for(Integer i=1; i<26; i++){
            set.add(i);
        }
        
        set.removeAll(slotSet);

        Integer index = new Random().nextInt(set.size());

        return (Integer) set.toArray()[index];
    }

    /**
     * @param index Slot reservieren
     * */
    public void lockSlot(Integer index){
    	slotSet.add(index);
    }

    /**
     * Alle Slots freigeben
     * */
    public void resetSlots(){
        this.slotSet.clear();
    }
}
