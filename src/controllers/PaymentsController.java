/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.RepositoryFactory;
import data.PaymentRepository;
import java.util.Map;
import models.Payment;

/**
 *
 * @author tiago
 */
public class PaymentsController extends AbstractController<Payment> {
    PaymentRepository repo;
    
    PaymentsController() {
        this.repo = RepositoryFactory.getPaymentRepository();
    }
    
    @Override
    public Payment newInstance(Map<String, Object> params) throws DataException {
        return new Payment(
                (String)params.get("status"),
                (Float)params.get("cost"),
                (String)params.get("date"),
                (int)params.get("paymentPlanId")
        );
    }
    
    @Override
    protected PaymentRepository getRepository() {
        return RepositoryFactory.getPaymentRepository();
    }
}
