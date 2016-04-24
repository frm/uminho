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
import models.SimpleMember;

/**
 *
 * @author mendes
 */
public class MemberRepository extends AbstractRepository<SimpleMember> {
    private static final String DB_TABLE = "Membro";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Name", "Nome");
       put("Kinship", "GrauParentesco");
       put("BirthDate", "DataNascimento");
       put("FamilyID", "Familia");
    }};

    public MemberRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected SimpleMember setObject(ResultSet result) throws DataException {
        SimpleMember s = new SimpleMember();
        try {
            s.setId( result.getInt( getColumnAttr("id") ) );
            s.setName( result.getString( getColumnAttr("name") ) );
            s.setKinship( result.getString( getColumnAttr("kinship") ));
            s.setBirthDate( result.getString( getColumnAttr("birthDate") ));
            s.setFamilyID( result.getInt( getColumnAttr("familyID") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving SimpleMember.");
        }
        return s;
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

