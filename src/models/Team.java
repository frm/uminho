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
    private int id;
    private String name;
    private int manager;
    private int volunteersNr;
    
    public Team(){
        this.id = -1;
        this.name = "Nothing here...";
        this.manager = -1;
        this.volunteersNr = -1;
    }

    public Team(String name, int manager, int volunteersNr) {
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

    public int getId() {
        return id;
    }
    
    public void setId(int id) {
        this.id = id;
    }


    public String getName() {
        return name;
    }

    public int getManager() {
        return manager;
    }

    public int getVolunteersNr() {
        return volunteersNr;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setManager(int manager) {
        this.manager = manager;
    }

    public void setVolunteersNr(int volunteersNr) {
        this.volunteersNr = volunteersNr;
    }
    
    @Override
    public Team clone(){
        return new Team(this);
    }
    
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\n");
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
