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
public class Team {
    private Integer id;
    private String name;
    private Integer manager;
    private Integer volunteersNr;
    
    public Team(){}

    public Team(String name, Integer manager, Integer volunteersNr) {
        this.id = -1;
        this.name = name;
        this.manager = manager;
        this.volunteersNr = volunteersNr;
    }
    
    public Team(Team t){
        this.id = t.getId();
        this.name = t.getName();
        this.manager = t.getManager();
        this.volunteersNr = t.getVolunteersNr();
    }

    public Integer getId() {
        return id;
    }
    
    public void setId(Integer id) {
        this.id = id;
    }


    public String getName() {
        return name;
    }

    public Integer getManager() {
        return manager;
    }

    public Integer getVolunteersNr() {
        return volunteersNr;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setManager(Integer manager) {
        this.manager = manager;
    }

    public void setVolunteersNr(Integer volunteersNr) {
        this.volunteersNr = volunteersNr;
    }
    
    @Override
    public Team clone(){
        return new Team(this);
    }
    
    public int hashCode()  {
        return id;
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(", ");
        sb.append(id);
        sb.append(", ");
        sb.append(name);
        sb.append(", ");
        sb.append(manager);
        sb.append(", ");
        sb.append(volunteersNr);
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Team t = (Team) o;
        
        return (this.name.equals(t.getName()) && this.manager == t.getManager() && this.volunteersNr == t.getVolunteersNr());
    }
    
}
