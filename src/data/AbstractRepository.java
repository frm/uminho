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
        boolean b;
        try {
            b = find(id) != null;
        } catch(DataException e) {
            b = false;
        }
        return b;
    }    
}
