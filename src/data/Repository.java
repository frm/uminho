/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.util.List;
import java.util.Map;

/**
 *
 * @author frmendes
 */
public interface Repository<T> {
    public void save(T entity) throws DataException;
    public T find(int id) throws DataException;
    public List<T> findBy(Map<String, Object> params) throws DataException;
}
