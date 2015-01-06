/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package data;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;
import models.Application;

/**
 *
 * @author mendes
 */
public class ApplicationRepository extends AbstractRepository<Application> {
    private static final String DB_TABLE = "Candidatura";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("ApplicationDate", "DataCandidatura");
       put("Location", "TerrenoConstrucao");
       put("Priority", "Prioridade");
       put("Status", "Estado");
       put("ApprovalDate", "DataAprovacao");
       put("File", "Ficheiro");
       put("Notes", "Observacoes");
       put("Manager", "FuncionarioResp");
       put("FamilyId", "Familia");
    }};

    public ApplicationRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Application setObject(ResultSet result) throws DataException {
        Application a = new Application();
        try {
            a.setId( result.getInt( getColumnAttr("id") ) );
            a.setApplicationDate( result.getString( getColumnAttr("applicationDate") ) );
            a.setLocation( result.getString( getColumnAttr("location") ) );
            a.setPriority( result.getInt( getColumnAttr("priority") ) );
            a.setStatus( result.getBoolean( getColumnAttr("status") ) );
            a.setApprovalDate( result.getString( getColumnAttr("aprovalDate") ) );
            a.setNotes( result.getString( getColumnAttr("notes") ) );
            a.setManager( result.getInt( getColumnAttr("manager") ) );
            a.setFamilyId( result.getInt( getColumnAttr("familyId") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving Application.");
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
