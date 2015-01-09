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
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import models.BasicModel;
import org.apache.commons.lang3.text.WordUtils;

/**
 *
 * @author frmendes
 * @param <T>
 */
abstract class AbstractRepository<T extends BasicModel> implements Repository<T> {

    private String url;
    private String username;
    private String password;
    private String DB_TABLE;
    private LinkedHashMap<String, String> COLUMN_ATTR;

    public AbstractRepository(String url, String username, String password, String DB_TABLE, LinkedHashMap columnAttrs) {
        this.url = url;
        this.username = username;
        this.password = password;
        this.DB_TABLE = DB_TABLE;
        this.COLUMN_ATTR = columnAttrs;
    }

    protected abstract T setObject(ResultSet result) throws DataException;
    protected abstract Set<String> getInsertIgnores();
    protected abstract Set<String> getUpdateIgnores();

    public Connection connect() throws SQLException {
        return DriverManager.getConnection(url, username, password);
    }
    
    public boolean attemptLogin() {
        try {
            DriverManager.getConnection(url, username, password);
            return true;
        } catch(SQLException e) {
            return false;
        }
    }
    
    public void saveAll(Collection<T> entities) throws DataException {
        for(T e : entities)
            save(e);
    }

    public Collection<T> findAll(Collection<Integer> ids) throws DataException {
        Collection<T> entities = new ArrayList<T>();
        for (int id : ids)
            entities.add( find(id) );

        return entities;
    }

    public boolean exists(int id) {
        try {
            return find(id) != null;
        } catch(DataException e) {
            return false;
        }
    }
    
    public void delete(int id) throws DataException {
        try {
            String query = new StringBuilder("DELETE FROM ")
                                            .append(DB_TABLE)
                                            .append(" WHERE id = ")
                                            .append(id)
                                            .append(";")
                                            .toString();
            
            Connection connection = null;
            PreparedStatement statement = null;
            try {
                connection = DriverManager.getConnection(url, username, password);
                statement = connection.prepareStatement(query);
                statement.executeUpdate();            
            } finally {
                statement.close();
                connection.close();
            }
        } catch (SQLException | NullPointerException e) {
            throw new DataException("Error deleting object from " + DB_TABLE + " with id " + id);
        }
    }

    public List<T> all() throws DataException {
        String query = new StringBuilder("SELECT * FROM ")
                        .append(DB_TABLE)
                        .append(";")
                        .toString();

        List<T> objects = new ArrayList<T>();
        try {
            Connection connection = DriverManager.getConnection(url, username, password);
            PreparedStatement statement = connection.prepareStatement(query);
            ResultSet result = statement.executeQuery();

            try {
                while ( result.next() ) {
                    objects.add( setObject(result) );
                }
            }
            finally {
                result.close();
                statement.close();
                connection.close();
            }

            return objects;
        } catch (SQLException e) {
            throw new DataException("Error retrieving all objects from " + DB_TABLE);
        }
    }

    @Override
     public T find(int id) throws DataException {
        try {
            T object;
            Connection connection = DriverManager.getConnection(url, username, password);
            PreparedStatement statement = connection.prepareStatement( getFindQuery(id) );
            ResultSet result = statement.executeQuery();

            try {
                if ( result.next() ) {
                    object = setObject(result);
                }
                else {
                    object = null;
                }
            }
            finally {
                result.close();
                statement.close();
                connection.close();
            }

            return object;
        } catch (SQLException e) {
            throw new DataException("Error finding object with id: " + id);
        }
    }

     /**
     *
     * @param t
     * @throws DataException
     */
    @Override
    public void save(T t) throws DataException {
        String query;
        int generatedKeys;

        if (t.getId() < 0) {
            query = getInsertQuery(t);
            generatedKeys = Statement.RETURN_GENERATED_KEYS;
        } else {
            query = getUpdateQuery(t);
            generatedKeys = Statement.NO_GENERATED_KEYS;
        }

        Connection connection;
        PreparedStatement statement;
        try {
            connection = DriverManager.getConnection(url, username, password);
            statement = connection.prepareStatement(query, generatedKeys);
            statement.executeUpdate();

            if(t.getId() > 0)
                return;

            ResultSet keys = statement.getGeneratedKeys();

            try {

                if( keys.next() ) {
                    t.setId( keys.getInt(1) );
                }
            } finally {
                keys.close();
                statement.close();
                connection.close();
            }
        }
        catch (SQLException ex) {
            throw new DataException("Error saving: " + t);
        }
    }

    private String getFindQuery(int id) {
        return new StringBuilder("SELECT * FROM ")
                                            .append(DB_TABLE)
                                            .append(" WHERE ")
                                            .append( getColumnAttr("id") )
                                            .append("=")
                                            .append(id)
                                            .append(";")
                                            .toString();
    }

    private String getUpdateQuery(T t) {
        StringBuilder query = new StringBuilder("UPDATE ")
                                                        .append(DB_TABLE)
                                                        .append(" SET ");

        Map<String, String> serializedObj = serialize(t, true, false, getUpdateIgnores());
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

    private String getInsertQuery(T t) {
        StringBuilder query = new StringBuilder("INSERT INTO ")
                                                .append(DB_TABLE)
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
    }

    @Override
    public List<T> findBy(Map<String, Object> params) throws DataException {
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

        List<T> objects = new ArrayList<T> ();

        try {
            Connection connection = DriverManager.getConnection(url, username, password);
            PreparedStatement statement = connection.prepareStatement(query.toString());
            ResultSet result = statement.executeQuery();

            try {
                while ( result.next() ) {
                    objects.add( setObject(result) );
                }
            }
            finally {
                result.close();
                statement.close();
                connection.close();
            }

            return objects;
        } catch (SQLException e) {
            throw new DataException("Error finding object with params: " + params.toString());
        }
    }

    private Map<String, String> serialize(T t, boolean includeId, boolean allowNull, Set<String> ignoredAttributes) {
        if (ignoredAttributes == null)
            ignoredAttributes = new HashSet<String>();

        LinkedHashMap<String, String> serialObj = new LinkedHashMap<>();

        for( Map.Entry<String, String> entry : COLUMN_ATTR.entrySet() ) {
            String attr = entry.getValue();

            if( (!includeId && attr.equals("id") ) || ignoredAttributes.contains(entry.getKey()) )
                continue;

            try {
                Method method = t.getClass().getMethod("get" + entry.getKey());
                Object result = method.invoke(t);

                if(result == null && !allowNull)
                    continue;

                String formatedValue = attributeToQuery(result);
                serialObj.put(attr, formatedValue);
            } catch (Exception e) {}
        }

        return serialObj;
    }

    private String attributeToQuery(Object attribute) {
        if (attribute == null) return "NULL";

        String value = attribute.toString();
        if (attribute instanceof String)
            value = String.format("'%s'", value);

        return value;
    }

    protected String getColumnAttr(String attr) {
        return COLUMN_ATTR.get( WordUtils.capitalize(attr) );
    }
}
