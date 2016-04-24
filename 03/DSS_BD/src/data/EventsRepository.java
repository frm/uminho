/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Set;
import models.Event;

/**
 *
 * @author paulo
 */
public class EventsRepository extends AbstractRepository<Event> {
    private static final String DB_TABLE = "Evento";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
        put("Id", "id");
        put("Date", "Data");
        put("AmountRaised", "ValorObtido");
        put("ParticipantsNr", "NrParticipantes");
        put("Observations", "Observacoes");
        put("Location","Local");
    }};
    
    public EventsRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }
 
    @Override
    protected Event setObject(ResultSet result) throws DataException {
        Event e = new Event();
        try {
            e.setId( result.getInt( getColumnAttr("id") ) );
            e.setDate( result.getString( getColumnAttr("date") ) );
            e.setLocation(result.getString (getColumnAttr("location")));
            e.setAmountRaised( result.getFloat( getColumnAttr("address") ) );
            e.setParticipantsNr( result.getInt( getColumnAttr("participantsNr") ) );
            e.setObservations( result.getString( getColumnAttr("observations") ) );
        } catch (SQLException ex) {
            throw new DataException("Error saving event.");
        }
        return e;
    }
    

    @Override
    protected Set<String> getInsertIgnores() {
        return null;
    }

    @Override
    protected Set<String> getUpdateIgnores() {
        return null;
    }    
    /*
    private String getInsertVolunteersEventQuery(T t) {
        StringBuilder query = new StringBuilder("INSERT INTO ")
                                                .append("")
                                                .append(" (");
        StringBuilder values = new StringBuilder("VALUES (");
        
        Map<String, String> serializedObj = serialize(t, false, true, getInsertIgnores());
        
        Iterator<Map.Entry<String, String>> entry = serializedObj.entrySet().iterator();
        while( entry.hasNext() ) {
            Map.Entry<String, String> e = entry.next();
            values.append( e.getValue() );
            query.append( e.getKey() );
            String s = entry.hasNext() ? ", " : ")";
            query.append(s);
            values.append(s);
        }
        
        return query.append(" ").append( values.append(";") ).toString();
    }*/
}

