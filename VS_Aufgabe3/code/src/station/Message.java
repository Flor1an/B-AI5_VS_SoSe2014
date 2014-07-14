package station;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;
import java.util.Date;

/**
 * Verwaltet eine Nachricht
 */
public class Message {

    private final static Integer messageLength = 34,
            typeOffset = 0,
            typeLength = 1,
            nameOffset = 1,
            nameLength = 10,
            dataOffset = 11,
            dataLength = 13,
            slotOffset = 25,
            slotLength = 1,
            dateOffset = 26,
            dateLength = 8;

    private byte[] data;

    /**
     * Konstruktor
     */
    public Message() {
        this.data = new byte[messageLength];
    }

    /**
     * @param type Stationstyp
     */
    public void setType(String type) {
        if (type.length() == 1) {
            byte[] bytes = type.getBytes();
            this.write(typeOffset, bytes);
        }
    }

    /**
     * @return Stationstyp
     */
    public String getType() {
        return new String(this.data, typeOffset, typeLength);
    }

    /**
     * @param name Stationsname
     */
    public void setName(String name) {
        if (name.length() <= nameLength) {
            byte[] bytes = name.getBytes();
            this.write(nameOffset, bytes);
        }
    }

    /**
     * @return Stationsname
     */
    public String getName() {
        return new String(this.data, nameOffset, nameLength);
    }

    /**
     * @param data Nachrichtendaten
     */
    public void setData(String data) {
        if (data.length() <= dataLength) {
            byte[] bytes = data.getBytes();
            this.write(dataOffset, bytes);
        }
    }

    /**
     * @return Nachrichtdaten
     */
    public String getData() {
        return new String(this.data, dataOffset, dataLength);
    }

    /**
     * @param slot Stationsslot
     */
    public void setSlot(Integer slot) {
        if (slot > -1 && slot < 26) {
            byte bytes = slot.byteValue();
            this.write(slotOffset, bytes);
        }
    }

    /**
     * @return Stationsslot
     */
    public Integer getSlot() {
        return Integer.parseInt(String.valueOf(this.data[slotOffset]));
    }

    /**
     * @param date Datum an dem die Nachricht gesendet wurde
     */
    public void setDate(Date date) {
        ByteBuffer buffer = ByteBuffer.allocate(8);
        buffer.order(ByteOrder.BIG_ENDIAN);
        buffer.putLong(date.getTime());
        this.write(dateOffset, buffer.array());
    }

    /**
     * @return Datum an dem die Nachricht gesendet wurde
     */
    public Date getDate() {
        byte[] bytes = Arrays.copyOfRange(this.data, dateOffset, dateOffset + dateLength);
        ByteBuffer buffer = ByteBuffer.allocate(8);
        buffer.order(ByteOrder.BIG_ENDIAN);
        buffer.put(bytes);
        buffer.flip();
        return new Date(buffer.getLong());
    }

    /**
     * Bytes in ByteArray schreiben
     */
    public void write(Integer offset, byte[] bytes) {
        for (int i = 0; i < bytes.length; i++) {
            this.write(offset + i, bytes[i]);
        }
    }

    /**
     * Byte in ByteArray schreiben
     */
    public void write(Integer index, byte data) {
        if (index > -1 && index < this.data.length) {
            this.data[index] = data;
        }
    }

    public byte[] getBytes(){
        return this.data;
    }

    public String toString() {
        return "Name: '" + this.getName() + "' " +
                "Typ: '" + this.getType() + "' " +
                "Data: '" + this.getData().length() + " Length' " +
                "Slot: '" + this.getSlot() + "' " +
                "Date: '" + this.getDate() + "'";
    }

}