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
import models.Employee;

/**
 *
 * @author tiago
 */
public class EmployeeRepository extends AbstractRepository<Employee> {

    private static final String DB_TABLE = "Funcionario";
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
       put("Salary", "Salario");
    }};

    public EmployeeRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Employee setObject(ResultSet result) throws DataException {
        Employee e = new Employee();
        try {
            e.setId( result.getInt( getColumnAttr("id") ) );
            e.setName( result.getString( getColumnAttr("name") ) );
            e.setAddress( result.getString( getColumnAttr("address") ) );
            e.setActivity( RepositoryFactory.getActivityRepository().find( result.getInt( getColumnAttr("activityID") ) ) );
            e.setNib( result.getString( getColumnAttr("nib") ) );
            e.setNif( result.getString( getColumnAttr("nif") ) );
            e.setBirthDate( result.getString( getColumnAttr("BirthDate") ) );
            e.setNationality( result.getString( getColumnAttr("nationality") ) );
            e.setCitizenship( result.getString( getColumnAttr("citizenship") ) );
            e.setMaritalStatus( result.getString( getColumnAttr("maritalStatus") ) );
            e.setSalary( result.getFloat( getColumnAttr("salary") ) );
        } catch (SQLException exc) {
            throw new DataException("Error saving volunteer.");
        }
        return e;
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
