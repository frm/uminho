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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import models.Volunteer;

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
    private static final HashMap<String, String> COLUMN_ATTR = new HashMap<String, String>() {{
       put("id", "id");
       put("name", "Nome");
       put("address", "Morada");
       put("nif", "NIF");
       put("nib", "NIB");
       put("birthdate", "DataNascimento");
       put("nationality", "Nacionalidade");
       put("citizenship", "Naturalidade");
       put("maritalStatus", "EstadoCivil");
       put("education", "Educacao");
       put("observations", "Observacoes");
       put("file", "Ficheiro");
       put("activity", "Atividade");
    }};
    
    public VolunteersRepository(String username, String password, String url) {
        this.username = username;
        this.password = password;
        this.url = url;
    }
    
    @Override
    public Volunteer find(int id) { return new Volunteer(); }
    
    /**
     *
     * @param v
     * @throws DataException
     */
    @Override
    public void save(Volunteer v) throws DataException {
        String query = null;
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
            
            statement.executeUpdate();
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
            query.append( COLUMN_ATTR.get((String) pair.getKey()) )
                 .append("=")
                 .append( attributeToQuery(pair.getValue()) );
            
            String nextLine = it.hasNext() ? " AND " : ";";
            query.append(nextLine);
        }
        
        System.out.println(query);
        
        return new ArrayList<Volunteer>();
    }
    private static String getUpdateQuery(Volunteer v) { return ""; }
    
    private static String getInsertQuery(Volunteer v) {
        return new StringBuilder("CALL ").append(INSERT_PROCEDURE)
                                                                 .append( serialize(v, false) )
                                                                 .append(";")
                                                                 .toString();
    }
            
    private static String serialize(Volunteer v, boolean includeId) {
        StringBuilder values = new StringBuilder("(");
        
        for( Map.Entry<String, String> entry : COLUMN_ATTR.entrySet() ) {
            if(!includeId && entry.getValue().equals("id")) continue;
            
            try {
                Method method = v.getClass().getMethod("get" + entry.getKey());
                Object result = method.invoke(v);
                if (result != null) {
                    String formatedValue = attributeToQuery(result);
                    values.append(formatedValue).append(", ");
                }
            } catch (Exception e) {}
        }
        
        return values.append(")").toString();
    }
    
    private static String attributeToQuery(Object attribute) {
        if (attribute == null) return "NULL";
        
        String value = attribute.toString();
        if (attribute instanceof String)
            value = String.format("'%s'", value);
        
        return value;
    }
}
