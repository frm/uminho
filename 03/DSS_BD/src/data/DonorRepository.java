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
import models.Donor;

/**
 *
 * @author pc14
 */
public class DonorRepository extends AbstractRepository<Donor>{
    private static final String DB_TABLE = "Doador";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
        put("Id", "id");
        put("Name", "Nome");
        put("Type", "Tipo");
        put("Address", "Morada");
        put("LastDonationDate", "DataUltimoDonativo");
        put("Nib", "NIB");
        put("Nif", "NIF");
        put("Observations", "Observacoes");
        put("ActivityID", "Atividade");
    }};
    
    public DonorRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }
    
    @Override
    protected Donor setObject(ResultSet result) throws DataException {
        Donor d = new Donor();
        try {
            d.setId( result.getInt( getColumnAttr("id") ) );
            d.setName( result.getString( getColumnAttr("name") ) );
            d.setType( result.getBoolean(getColumnAttr("type")));
            d.setAddress( result.getString( getColumnAttr("address")));
            d.setLastDonationDate( result.getString( getColumnAttr("lastDonationDate")));
            d.setActivity( (Activity) RepositoryFactory.getActivityRepository().find( result.getInt( getColumnAttr("activityID") ) ) );
            d.setNib( result.getString( getColumnAttr("nib") ) );
            d.setNif( result.getString( getColumnAttr("nif") ) );
            d.setObservations( result.getString( getColumnAttr("observations")));           
        } catch (SQLException e) {
            throw new DataException("Error saving representative.");
        }
        return d;
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
