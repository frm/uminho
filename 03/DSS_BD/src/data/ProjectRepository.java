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
import models.Project;

/**
 *
 * @author Tiago
 */
 
public class ProjectRepository extends AbstractRepository<Project> {

    private static final String DB_TABLE = "Projeto";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Name", "Nome");
       put("StartDate", "DataInicio");
       put("Budget", "Orcamento");
       put("Eta", "DataFinalPrevista");
       put("EndDate", "DataFinal");
       put("SignDate", "DataAssinContr");
       put("DeliveryDate", "DataEntrgChave");
       put("FinalCost", "CustoFinal");
       put("Notes", "Observacoes");
       put("ApplicationId", "Candidatura");
       put("PaymentPlanId", "PlanoPagamentos");
    }};

    public ProjectRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Project setObject(ResultSet result) throws DataException {
        Project p = new Project();
        try {
            p.setId( result.getInt( getColumnAttr("id") ) );
            p.setName( result.getString( getColumnAttr("name") ) );
            p.setStartDate( result.getString( getColumnAttr("startDate") ) );
            p.setBudget( result.getFloat( getColumnAttr("budget") ) );
            p.setEta( result.getString( getColumnAttr("eta") ) );
            p.setEndDate( result.getString( getColumnAttr("endDate") ) );
            p.setSignDate( result.getString( getColumnAttr("signDate") ) );
            p.setDeliveryDate( result.getString( getColumnAttr("deliveryDate") ) );
            p.setFinalCost( result.getFloat( getColumnAttr("finalCost") ) );
            p.setNotes( result.getString( getColumnAttr("notes") ) );
            p.setApplicationId( result.getInt( getColumnAttr("id") ) );
            p.setPaymentPlanId( result.getInt( getColumnAttr("id") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving project.");
        }
        return p;
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
