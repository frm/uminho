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
import models.PaymentPlan;

/**
 *
 * @author tiago
 */
public class PaymentPlanRepository extends AbstractRepository<PaymentPlan> {

    private static final String DB_TABLE = "PlanoPagamentos";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("NextPayment", "ProximaPrestacao");
       put("Notes", "Observacoes");
    }};

    public PaymentPlanRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected PaymentPlan setObject(ResultSet result) throws DataException {
        PaymentPlan pp = new PaymentPlan();
        try {
            pp.setId( result.getInt( getColumnAttr("id") ) );
            pp.setnextPayment( result.getFloat( getColumnAttr("nextPayment") ) );
            pp.setNotes( result.getString( getColumnAttr("notes") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving paymentPlan.");
        }
        return pp;
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
