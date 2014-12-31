/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import models.Volunteer;
import org.apache.commons.lang3.text.WordUtils;

/**
 *
 * @author frmendes
 */
public class VolunteersRepository extends AbstractRepository<Volunteer> {

    private String username;
    private String password;
    private String url;

    private static final String DB_TABLE = "Voluntario";
    private static final String INSERT_PROCEDURE = "sp_insert_volunteer";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Name", "Nome");
       put("Address", "Morada");
       put("Nif", "NIF");
       put("Nib", "NIB");
       put("BirthDate", "DataNascimento");
       put("Nationality", "Nacionalidade");
       put("Citizenship", "Naturalidade");
       put("MaritalStatus", "EstadoCivil");
       put("Education", "Educacao");
       put("Observations", "Observacoes");
       put("File", "Ficheiro");
       put("Activity", "Atividade");
    }};

    public VolunteersRepository(String username, String password, String url) {
        this.username = username;
        this.password = password;
        this.url = url;
    }

    @Override
    public Volunteer find(int id) throws DataException {
        try {
            Volunteer v;
            Connection connection = DriverManager.getConnection(url, username, password);
            PreparedStatement statement = connection.prepareStatement( getFindQuery(id) );
            ResultSet result = statement.executeQuery();
            
            try {
                if ( result.next() ) {
                    v = new Volunteer();
                    v.setId( result.getInt( getColumnAttr("id") ) );
                    v.setName( result.getString( getColumnAttr("name") ) );
                    v.setAddress( result.getString( getColumnAttr("address") ) );
                    v.setNib( result.getString( getColumnAttr("nib") ) );
                    v.setNif( result.getString( getColumnAttr("nif") ) );
                    v.setBirthDate( result.getDate( getColumnAttr("birthDate") ) );
                    v.setNationality( result.getString( getColumnAttr("nationality") ) );
                    v.setCitizenship( result.getString( getColumnAttr("citizenship") ) );
                    v.setMaritalStatus( result.getString( getColumnAttr("maritalStatus") ) );
                    v.setObservations( result.getString( getColumnAttr("observations") ) );
                    v.setFile( result.getString( getColumnAttr("file") ) );
                    }
                else {
                    v = null;
                }
            }
            finally {
                result.close();
                statement.close();
                connection.close();
            }
            
            return v;
        } catch (Exception e) {
            throw new DataException("Error finding volunteer with id: " + id);
        }
    }
    
    /**
     *
     * @param v
     * @throws DataException
     */
    @Override
    public void save(Volunteer v) throws DataException {
        String query;
        int generatedKeys;

        if (v.getId() < 0) {
            query = getInsertQuery(v);
            generatedKeys = Statement.RETURN_GENERATED_KEYS;
        } else {
            query = getUpdateQuery(v);
            generatedKeys = Statement.NO_GENERATED_KEYS;
        }

        Connection connection;
        PreparedStatement statement;
        try {
            connection = DriverManager.getConnection(url, username, password);
            statement = connection.prepareStatement(query, generatedKeys);
            
            System.out.println(query);
            statement.executeUpdate();
            System.out.println("\n\nYAY");
            ResultSet keys = statement.getGeneratedKeys();
            

            try {
                if( keys.next() ) {
                    v.setId( (int)keys.getLong(1) );
                } else {
                    throw new DataException("Error getting id for: " + v);
                }
            } finally {
                keys.close();
                statement.close();
                connection.close();
            }
        }
        catch (SQLException ex) {
            throw new DataException("Error saving volunteer: " + v);
        }
    }
        // TODO: add DAO sql query here
    @Override
    public List<Volunteer> findBy(Map<String, Object> params) {
        StringBuilder query = new StringBuilder();
        query.append("SELECT * FROM ")
             .append(DB_TABLE)
             .append(" WHERE ");

        Iterator it = params.entrySet().iterator();
        while(it.hasNext()) {
            Map.Entry pair = (Map.Entry)it.next();
            query.append( getColumnAttr( (String) pair.getKey() ) )
                 .append("=")
                 .append( attributeToQuery( pair.getValue() ) );

            String nextLine = it.hasNext() ? " AND " : ";";
            query.append(nextLine);
        }

        System.out.println(query);

        return new ArrayList<Volunteer>();
    }
    
    public static String getUpdateQuery(Volunteer v) {
        StringBuilder query = new StringBuilder("UPDATE ")
                                                        .append(DB_TABLE)
                                                        .append(" SET ");
        
        Map<String, String> serializedObj = serialize(v, true, false, new HashSet<String>(){{ add("Activity"); }});
        Iterator<Map.Entry<String, String>> entry = serializedObj.entrySet().iterator();
        
        while( entry.hasNext() ) {
            Map.Entry<String, String> attr = entry.next();
            
            if(attr.getKey().equals(getColumnAttr("id"))) continue;
            
            query.append(attr.getKey())
                            .append("=")
                            .append(attr.getValue());
            
            String s = entry.hasNext() ? ", " : " ";
            query.append(s);
        }
        
        query.append("WHERE ")
                    .append(getColumnAttr("id"))
                    .append("=")
                    .append(serializedObj.get(getColumnAttr("id")))
                    .append(";");
        
        return query.toString();
    }

    public static String getInsertQuery(Volunteer v) {
        StringBuilder sp = new StringBuilder("CALL ")
                                                .append(INSERT_PROCEDURE)
                                                .append("(");
        
        Map<String, String> serializedObj = serialize(v, false, true, null);
        
        Iterator<Map.Entry<String, String>> entry = serializedObj.entrySet().iterator();
        while( entry.hasNext() ) {
            sp.append( entry.next().getValue() );
            String s = entry.hasNext() ? ", " : ");";
            sp.append(s);
        }
        
        return sp.toString();
    }
    
    public static String getFindQuery(int id) {
        return new StringBuilder("SELECT * FROM ")
                                                        .append(DB_TABLE)
                                                        .append(" WHERE ")
                                                        .append( getColumnAttr("id") )
                                                        .append("=")
                                                        .append(id)
                                                        .append(";")
                                                        .toString();
    }

    private static Map<String, String> serialize(Volunteer v, boolean includeId, boolean allowNull, Set<String> ignoredAttributes) {
        if (ignoredAttributes == null)
            ignoredAttributes = new HashSet<String>();
        
        LinkedHashMap<String, String> serialObj = new LinkedHashMap<>();

        for( Map.Entry<String, String> entry : COLUMN_ATTR.entrySet() ) {
            String attr = entry.getValue();
            
            if( (!includeId && attr.equals("id") ) || ignoredAttributes.contains(entry.getKey()) )
                continue;

            try {
                Method method = v.getClass().getMethod("get" + entry.getKey());
                Object result = method.invoke(v);
                
                if(result == null && !allowNull)
                    continue;
                
                String formatedValue = attributeToQuery(result);
                serialObj.put(attr, formatedValue);
            } catch (Exception e) {}
        }

        return serialObj;
    }

    private static String attributeToQuery(Object attribute) {
        if (attribute == null) return "NULL";

        String value = attribute.toString();
        if (attribute instanceof String)
            value = String.format("'%s'", value);

        return value;
    }

    private static String getColumnAttr(String attr) {
        return COLUMN_ATTR.get( WordUtils.capitalize(attr) );
    }
    
    public static void main(String args[]) throws DataException {
        // To run this put getInsertQuery and getUpdateQuery to public
        VolunteersRepository repo = new VolunteersRepository("habitat_admin", "testuser123", "jdbc:mysql://localhost:3306/habitat");
        Volunteer v = new Volunteer();
        Map query = new HashMap<String, Object>() {{
            put("id", 1);
            put ("name", "myName");
            put("address", "here");
        }};
        
        repo.findBy(query);
        System.out.println(VolunteersRepository.getInsertQuery(v));
        
        Volunteer v2 = new Volunteer("myName", "myAddress", null, "nib", "myActivity",
            new TreeSet<Integer>(), null, "education", "nacionality", "citizenship",
            "maritalStatus", "observations", "file");
        System.out.println(VolunteersRepository.getUpdateQuery(v2));
        System.out.println(VolunteersRepository.getFindQuery(0));
        
        System.out.println(repo.find(1));
        v2.setId(1);
        repo.save(v2);
        System.out.println(repo.find(1));
    }
}
