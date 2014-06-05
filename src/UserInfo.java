
/**
 *
 * @author joaorodrigues
 */

import java.io.Serializable;
import java.util.GregorianCalendar;

/**
 *
 * @author joaorodrigues
 */
public class UserInfo implements Serializable{
    private boolean gender;
    private double height;
    private double weight;
    private GregorianCalendar birthDate;
    private String favoriteSport;

    /**
     *
     */
    public UserInfo(){
        this.gender = true;
        this.height = 0.0;
        this.weight = 0.0;
        this.birthDate = new GregorianCalendar();
        this.favoriteSport = "";
    }

    /**
     *
     * @param gender
     * @param height
     * @param weight
     * @param birthDate
     * @param favoriteSport
     */
    public UserInfo(boolean gender,double height, double weight, GregorianCalendar birthDate, String favoriteSport){
        this.gender = gender;
        this.height = height;
        this.weight = weight;
        this.birthDate = (GregorianCalendar) birthDate.clone();
        this.favoriteSport = favoriteSport;
    }

    /**
     *
     * @param user
     */
    public UserInfo(UserInfo user){
        this.gender = user.getGender();
        this.height = user.getHeight();
        this.weight = user.getWeight();
        this.birthDate = user.getBirthDate();
        this.favoriteSport = user.getFavoriteSport();
    }

    void setGender(boolean gender){this.gender = gender;}
    void setHeight(double height){this.height = height;}
    void setWeight(double weight){this.weight = weight;}
    void setBirthDate(GregorianCalendar birthDate){this.birthDate = (GregorianCalendar) birthDate.clone();}
    void setFavoriteSport(String favoriteSport){this.favoriteSport = favoriteSport;}

    boolean getGender(){return this.gender;}
    double getHeight(){return this.height;}
    double getWeight(){return this.weight;}
    GregorianCalendar getBirthDate(){return (GregorianCalendar) (this.birthDate).clone();}
    String getFavoriteSport(){  return this.favoriteSport;}

    @Override
    public String toString(){
        StringBuilder result = new StringBuilder();

        result.append("\nGender: ");
        if (gender) result.append("Male ");
        else result.append("Female ");

        result.append("\nHeight: ");
        result.append(this.height);
        result.append("\nWeight: ");
        result.append(this.weight);
        result.append("\nBirth Date: ");
        result.append( Scan.dateFormat(this.birthDate) );
        result.append("\nFavorite Sport: ");
        result.append( this.favoriteSport);
        return result.toString();
    }

    @Override
    public UserInfo clone(){
        return new UserInfo(this);
    }

    @Override
    public boolean equals(Object o){
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;

        UserInfo u = (UserInfo) o;

        return (u.getGender() == this.gender
            && u.getHeight() == this.height
            && u.getWeight() == this.weight
            && (u.getBirthDate() ).equals(this.birthDate)
            && ( u.getFavoriteSport() ).equals( this.favoriteSport ));
    }

    /**
     *
     * @param u
     * @param ui
     * @return
     */
    public static UserInfo generateValidInfo(UserInfo u, UserInfo ui) {
        UserInfo n = new UserInfo(u);
        
        if (ui.getWeight() != 0)
            n.setWeight( ui.getWeight() );
        if (ui.getHeight() != 0)
            n.setHeight( ui.getHeight() );
        if (ui.getFavoriteSport().length() != 0)
            n.setFavoriteSport( ui.getFavoriteSport() );

        return n;
    }
}
