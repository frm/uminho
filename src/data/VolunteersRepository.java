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
       put("SQLBirthDate", "DataNascimento");
       put("Nationality", "Nacionalidade");
       put("Citizenship", "Naturalidade");
       put("MaritalStatus", "EstadoCivil");
       put("Education", "Escolaridade");
       put("Observations", "Observacoes");
       put("File", "Ficheiro");
       put("Activity", "Atividade");
    }};

    public VolunteersRepository(String username, String password, String url) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Volunteer setObject(ResultSet result) throws DataException {
        Volunteer v = new Volunteer();
        try {
            v.setId( result.getInt( getColumnAttr("id") ) );
            v.setName( result.getString( getColumnAttr("name") ) );
            v.setAddress( result.getString( getColumnAttr("address") ) );
            v.setActivity( Integer.toString( result.getInt( getColumnAttr("activity") ) ) );
            v.setNib( result.getString( getColumnAttr("nib") ) );
            v.setNif( result.getString( getColumnAttr("nif") ) );
            v.setBirthDate( result.getDate( getColumnAttr("SQLBirthDate") ) );
            v.setNationality( result.getString( getColumnAttr("nationality") ) );
            v.setCitizenship( result.getString( getColumnAttr("citizenship") ) );
            v.setMaritalStatus( result.getString( getColumnAttr("maritalStatus") ) );
            v.setObservations( result.getString( getColumnAttr("observations") ) );
            v.setFile( result.getString( getColumnAttr("file") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving volunteer.");
        }
        return v;
    }

    @Override
    protected Set<String> getInsertIgnores() {
        return new HashSet<String>() {{ add("Activity"); }};
    }

    @Override
    protected Set<String> getUpdateIgnores() {
        return new HashSet<String>() {{ add("Activity"); }};
    }
}
