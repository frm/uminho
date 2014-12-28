/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author tiago
 */
public class Material {
    private int id;
    private String name;
    private int quantity;

    public Material() {
        this.id = -1;
        this.name = "Nothing here...";
        this.quantity = -1;
    }

    public Material(String name, int quantity) {
        this.id = -1;
        this.name = name;
        this.quantity = quantity;
    }
    
    public Material(Material m){
        this.id = m.getId();
        this.name = m.getName();
        this.quantity = m.getQuantity();
    }

    public int getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public int getQuantity() {
        return quantity;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setQuantity(int quantity) {
        this.quantity = quantity;
    }
    
    @Override
    public Material clone(){
        return new Material(this);
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("ID: ");
        sb.append(id);
        sb.append("Nome: ");
        sb.append(name);
        sb.append("Quantidade: ");
        sb.append(quantity);
        return sb.toString();
    }
    
    @Override
    public boolean equals (Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Material m = (Material) o;
    
        return (this.id == m.getId() && this.name.equals(m.getName()) && this.quantity == m.getQuantity());
    }
}
