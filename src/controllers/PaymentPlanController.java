/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.PaymentPlanRepository;
import data.RepositoryFactory;
import java.util.Map;
import models.PaymentPlan;

/**
 *
 * @author tiago
 */
public class PaymentPlanController extends AbstractController<PaymentPlan> {
    PaymentPlanRepository repo;
    
    PaymentPlanController() {
        this.repo = RepositoryFactory.getPaymentPlanRepository();
    }
    
    @Override
    public PaymentPlan newInstance(Map<String, Object> params) throws DataException {
        return new PaymentPlan(
                (Float)params.get("nextPayment"),
                (String)params.get("notes")          
        );
    }
    
    @Override
    protected PaymentPlanRepository getRepository() {
        return RepositoryFactory.getPaymentPlanRepository();
    }
}
