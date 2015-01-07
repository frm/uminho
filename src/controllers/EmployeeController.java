/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import data.EmployeeRepository;
import data.RepositoryFactory;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import models.Activity;
import models.Contact;
import models.Employee;

/**
 *
 * @author tiago
 */
public class EmployeeController extends AbstractController<Employee> {
    EmployeeRepository repo;
    
    EmployeeController() {
        this.repo = RepositoryFactory.getEmployeeRepository();
    }
    
    @Override
    public Employee newInstance(final Map<String, Object> params) throws DataException {
        return new Employee(
                (String)params.get("name"),
                (String)params.get("address"),
                (String)params.get("nif"),
                (String)params.get("nib"),
                (Activity)ControllerFactory.getActivityController().findBy( new HashMap<String, Object>() {{
                    put("name", params.get("activity"));}}),
                (Set<Contact>)params.get("contacts"),
                (String)params.get("username"),
                (String)params.get("birthDate"),
                (String)params.get("education"),
                (String)params.get("nationality"),
                (String)params.get("citizenship"),
                (String)params.get("maritalStatus"),
                (Float)params.get("salary")         
        );
    }
    
    protected EmployeeRepository getRepository() {
        return RepositoryFactory.getEmployeeRepository();
    }
}
