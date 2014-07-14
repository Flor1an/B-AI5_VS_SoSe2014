package manager;

import java.util.ArrayList;
import java.util.Date;

/**
 * Klasse zur Verwaltung der Systemzeit (Singleton)
 * */
public class ClockManager {

    private static ClockManager instance = new ClockManager();

    private Long initOffset;
    private Long syncOffset;
    private Long frameStart;
    public Long accTime;
    public Integer accCount;
    private ArrayList<Date> clocksToSync;

    /**
     * Konstruktor
     * */
    private ClockManager(){
        this.initOffset = 0L;
        this.syncOffset = 0L;
        this.frameStart = 0L;
        this.accTime = 0L;
        this.accCount = 1;
        this.clocksToSync = new ArrayList<Date>();
    }

    /**
     * Aktuelle (einzige) Inszanz bekommen
     * */
    public static ClockManager getInstance(){
        return ClockManager.instance;
    }

    /**
     * Berechnungsgrundlage für Systemzeite zurücksetzen
     * */
    public void resetClockToSync(){
        this.clocksToSync.clear();
    }

    /**
     * Festen, initialen Offset
     * */
    public void setSyncOffset(Long offset){
        this.syncOffset = offset;
    }

    /**
     * Festen, initialen Offset
     * */
    public Long getSyncOffset(){
        return this.syncOffset;
    }

    /**
     * Festen, initialen Offset
     * */
    public void setframeStart(Long now) {
        this.frameStart = now;
    }

    /**
     * Festen, initialen Offset
     * */
    public Long getFrameStart() {
        return frameStart;
    }

    /**
     * @return Aktuelle Zeit zurückgeben
     * */
    public Long now(){
        return System.currentTimeMillis() + this.syncOffset;
    }

}
