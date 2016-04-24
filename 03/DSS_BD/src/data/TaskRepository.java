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
import models.Task;

/**
 *
 * @author tiago
 */
public class TaskRepository extends AbstractRepository<Task> {

    private static final String DB_TABLE = "Tarefa";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Name", "Nome");
       put("StartDate", "DataInicio");
       put("EndDate", "DataFinal");
       put("Status", "Estado");
       put("ProjectId", "Projeto");
    }};

    public TaskRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Task setObject(ResultSet result) throws DataException {
        Task t = new Task();
        try {
            t.setId( result.getInt( getColumnAttr("id") ) );
            t.setName( result.getString( getColumnAttr("name") ) );
            t.setStartDate( result.getString( getColumnAttr("startDate") ) );
            t.setEndDate( result.getString( getColumnAttr("endDate") ) );
            t.setStatus( result.getString( getColumnAttr("status") ) );
            t.setProjectId( result.getInt( getColumnAttr("projectId") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving task.");
        }
        return t;
    }

    @Override
    protected Set<String> getInsertIgnores() {
        return null;
    }

    @Override
    protected Set<String> getUpdateIgnores() {
        return null;
    }
}

