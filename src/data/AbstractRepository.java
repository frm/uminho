/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 *
 * @author frmendes
 */
public abstract class AbstractRepository<T> implements Repository<T> {
    
    public abstract String getTableName();
    
    public abstract String getColumnFor(String attribute);
    
    public void saveAll(Collection<T> entities) {
        for(T e : entities)
            save(e);
    }
    
    public Collection<T> findAll(Collection<Integer> ids) {
        Collection<T> entities = new ArrayList<T>();
        for (int id : ids)
            entities.add( find(id) );
        
        return entities;
    }
    
    public boolean exists(int id) {
        return find(id) != null;
    }
    
    // TODO: add DAO sql query here
    @Override
    public List<T> findBy(Map<String, Object> params) {
        StringBuilder query = new StringBuilder();
        query.append("SELECT * FROM ")
             .append(getTableName())
             .append(" WHERE ");
        
        Iterator it = params.entrySet().iterator();
        while(it.hasNext()) {
            Map.Entry pair = (Map.Entry)it.next();
            query.append( getColumnFor((String) pair.getKey()) )
                 .append("=")
                 .append( attributeToQuery(pair.getValue()) );
            
            String nextLine = it.hasNext() ? " AND " : ";";
            query.append(nextLine);
        }
        
        System.out.println(query);
        
        return new ArrayList<T>();
    }
    
    private static String attributeToQuery(Object attribute) {
        String value = attribute.toString();
        if (attribute instanceof String)
            value = String.format("'%s'", value);
        
        return value;
    }
    
}
