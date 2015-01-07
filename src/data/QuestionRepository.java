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
import models.Question;

/**
 *
 * @author mendes
 */
public class QuestionRepository extends AbstractRepository<Question> {
    private static final String DB_TABLE = "Pergunta";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Text", "PergTexto");
       put("Enabled", "Ativa");
    }};

    public QuestionRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Question setObject(ResultSet result) throws DataException {
        Question q = new Question();
        try {
            q.setId( result.getInt( getColumnAttr("id") ) );
            q.setText( result.getString( getColumnAttr("text") ) );
            q.setEnabled( result.getBoolean( getColumnAttr("enabled") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving question.");
        }
        return q;
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
