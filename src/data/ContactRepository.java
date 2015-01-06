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
import models.Contact;

/**
 *
 * @author mendes
 */
public class ContactRepository extends AbstractRepository<Contact> {
    private static final String DB_TABLE = "Contacto";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Value", "Valor");
       put("Type", "Tipo");
       put("OwnerType", "TipoDono");
       put("Owner", "Dono");
    }};

    public ContactRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Contact setObject(ResultSet result) throws DataException {
        Contact c = new Contact();
        try {
            c.setId( result.getInt( getColumnAttr("id") ) );
            c.setType( result.getString( getColumnAttr("type") ) );
            c.setValue( result.getString( getColumnAttr("value") ) );
            c.setOwnerType( result.getString( getColumnAttr("ownerType") ) );
            c.setOwner( result.getInt( getColumnAttr("owner") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving Contact.");
        }
        return c;
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
