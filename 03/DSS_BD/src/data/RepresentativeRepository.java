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
import models.Representative;

/**
 *
 * @author mendes
 */
public class RepresentativeRepository extends AbstractRepository<Representative> {
    private static final String DB_TABLE = "Representante";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Name", "Nome");
       put("Nif", "NIF");
       put("Nib", "NIB");
       put("BirthDate", "DataNascimento");
       put("Nationality", "Nacionalidade");
       put("BirthPlace", "Naturalidade");
       put("MaritalStatus", "EstadoCivil");
       put("Education", "Escolaridade");
       put("Observations", "Observacoes");
       put("ActivityId", "Atividade");
       put("FamilyID", "Familia");
    }};

    public RepresentativeRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Representative setObject(ResultSet result) throws DataException {
        Representative r = new Representative();
        try {
            r.setId( result.getInt( getColumnAttr("id") ) );
            r.setName( result.getString( getColumnAttr("name") ) );
            r.setActivity((Activity) RepositoryFactory.getActivityRepository().find( result.getInt( getColumnAttr("activityId") ) ) );
            r.setNib( result.getString( getColumnAttr("nib") ) );
            r.setNif( result.getString( getColumnAttr("nif") ) );
            r.setBirthDate( result.getString( getColumnAttr("birthDate") ) );
            r.setNationality( result.getString( getColumnAttr("nationality") ) );
            r.setBirthPlace( result.getString( getColumnAttr("birthPlace") ) );
            r.setMaritalStatus( result.getString( getColumnAttr("maritalStatus") ) );
            r.setFamilyID( result.getInt( getColumnAttr("familyID") ) );
            r.setEducation( result.getString( getColumnAttr("education") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving representative.");
        }
        return r;
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
