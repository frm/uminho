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
import models.Volunteer;

/**
 *
 * @author frmendes
 */
public class VolunteersRepository extends AbstractRepository<Volunteer> {

    private static final String DB_TABLE = "Voluntario";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Name", "Nome");
       put("Address", "Morada");
       put("Nif", "NIF");
       put("Nib", "NIB");
       put("BirthDate", "DataNascimento");
       put("Nationality", "Nacionalidade");
       put("Citizenship", "Naturalidade");
       put("MaritalStatus", "EstadoCivil");
       put("Education", "Escolaridade");
       put("Observations", "Observacoes");
       put("ActivityID", "Atividade");
       put("CurrentTeam", "EquipaAtual");
       put("donorId", "Doador");
    }};

    public VolunteersRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Volunteer setObject(ResultSet result) throws DataException {
        Volunteer v = new Volunteer();
        try {
            v.setId( result.getInt( getColumnAttr("id") ) );
            v.setName( result.getString( getColumnAttr("name") ) );
            v.setAddress( result.getString( getColumnAttr("address") ) );
            v.setActivity( (Activity) RepositoryFactory.getActivityRepository().find( result.getInt( getColumnAttr("activityID") ) ) );
            v.setNib( result.getString( getColumnAttr("nib") ) );
            v.setNif( result.getString( getColumnAttr("nif") ) );
            v.setBirthDate( result.getString( getColumnAttr("BirthDate") ) );
            v.setNationality( result.getString( getColumnAttr("nationality") ) );
            v.setCitizenship( result.getString( getColumnAttr("citizenship") ) );
            v.setMaritalStatus( result.getString( getColumnAttr("maritalStatus") ) );
            v.setObservations( result.getString( getColumnAttr("observations") ) );
            v.setDonorId( result.getInt( getColumnAttr("donorId") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving volunteer.");
        }
        return v;
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
