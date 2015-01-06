/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.AbstractRepository;
import data.DataException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import models.BasicModel;

/**
 *
 * @author mendes
 */
public abstract class AbstractController<T extends BasicModel> implements Controller<T> {
    public List<T> saveAll(List<Map<String, Object>> ts) throws DataException {
        List<T> result = new ArrayList<T>();
        
        for(Map<String, Object> t : ts)
            result.add(this.save(t));
        
        return result;
    }
    
    @Override
    public T save(Map<String, Object> params) throws DataException {
        T t = newInstance(params);
        getRepository().save(t);
        return t;
    }
    
    @Override
    public T find(int id) throws DataException {
        return getRepository().find(id);
    }
    
    public List<T> findBy(Map<String, Object> params) throws DataException {
        return getRepository().findBy(params);
    }
    
    public List<T> all() throws DataException {
        return getRepository().all();
    }
    
    protected abstract AbstractRepository<T> getRepository();
}
