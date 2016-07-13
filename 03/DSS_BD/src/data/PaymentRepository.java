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
import models.Payment;

/**
 *
 * @author frmendes
 */
public class PaymentRepository extends AbstractRepository<Payment> {

    private static final String DB_TABLE = "Prestacao";
    private static final LinkedHashMap<String, String> COLUMN_ATTR = new LinkedHashMap<String, String>() {{
       put("Id", "id");
       put("Date", "Data");
       put("Cost", "Valor");
       put("Status", "Estado");
       put("PaymentPlanId","PlanoPagamentos");
    }};

    public PaymentRepository(String url, String username, String password) {
        super(url, username, password, DB_TABLE, COLUMN_ATTR);
    }

    @Override
    protected Payment setObject(ResultSet result) throws DataException {
        Payment p = new Payment();
        try {
            p.setId( result.getInt( getColumnAttr("id") ) );
            p.setDate( result.getString( getColumnAttr("date") ) );
            p.setCost( result.getFloat( getColumnAttr("cost") ) );
            p.setStatus( result.getString( getColumnAttr("status") ) );
            p.setPaymentPlanId( result.getInt( getColumnAttr("paymentPlanId") ) );
        } catch (SQLException e) {
            throw new DataException("Error saving payment.");
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
