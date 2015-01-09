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
import models.Activity;

/**
 *
 * @author mendes
 */
public class ActivityRepository extends AbstractRepository<Activity> {
    private static final String DB_TABLE = "Atividade";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Name", "Nome");
    }};

    public ActivityRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Activity setObject(ResultSet result) throws DataException {
        Activity a = new Activity();
        try {
            a.setId( result.getInt( getColumnAttr("id") ) );
            a.setName( result.getString( getColumnAttr("name") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving activity.");
        }
        return a;
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
