import java.util.GregorianCalendar;

public class UserDraft{
    private int id;
    private String email;
    private String password;
    private String name;
    //false for male, true for female
    private boolean gender;
    private double height;
    private double weight;
    private GregorianCalendar birthDate;
    private String favoriteSport;
    //Save friends from Hitler.


    public UserDraft(){
        this.email = "";
        this.password = "";
        this.name = "";
        this.gender = true;
        this.height = 0.0;
        this.weight = 0.0;
        this.birthDate = new GregorianCalendar();
        this.favoriteSport = "";
    }

    public UserDraft(String email, String password, String name,boolean gender,double height, double weight, GregorianCalendar birthDate, String favoriteSport){
        this.email = email;
        this.password = password;
        this.name = name;
        this.gender = gender;
        this.height = height;
        this.weight = weight;
        this.birthDate = (GregorianCalendar) birthDate.clone();
        this.favoriteSport = favoriteSport;
    }

    public UserDraft(UserDraft user){
        this.email = user.getEmail();
        this.password = user.getPassword();
        this.name = user.getName();
        this.gender = user.getGender();
        this.height = user.getHeight();
        this.weight = user.getWeight();
        this.birthDate = user.getBirthDate();
        this.favoriteSport = user.getFavoriteSport();
    }

    private void setId(int id){this.id = id;}
    void setEmail(String email){this.email = email;}
    void setPassword(String password){this.password = password;}
    void setName(String name){this.name = name;}
    void setGender(boolean gender){this.gender = gender;}
    void setHeight(double height){this.height = height;}
    void setWeight(double weight){this.weight = weight;}
    void setBirthDate(GregorianCalendar birthDate){this.birthDate = (GregorianCalendar) birthDate.clone();}
    void setFavoriteSport(String favoriteSport){this.favoriteSport = favoriteSport;}

    int getId(){return this.id;}
    String getEmail(){return this.email;}
    String getPassword(){return this.password;}
    String getName(){return this.name;}
    boolean getGender(){return this.gender;}
    double getHeight(){return this.height;}
    double getWeight(){return this.weight;}
    GregorianCalendar getBirthDate(){return (GregorianCalendar) (this.birthDate).clone();}
    String getFavoriteSport(){  return this.favoriteSport;}

    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("Utilizador: ");
        result.append("\nE-Mail: ");
        result.append(this.email);
        result.append("\nName: ");
        result.append(this.name);
        result.append("\nGender: ");
        if(gender) result.append("Female ");
        else result.append("Male ");

        result.append("\nHeight: ");
        result.append(this.height);
        result.append("\nWeight: ");
        result.append(this.weight);
        result.append("\nBirth Date: ");
        result.append( (this.birthDate).toString());
        result.append("\nFavorite Sport: ");
        result.append( this.favoriteSport);
        return result.toString();
    }

    public UserDraft clone(){
        return new UserDraft(this);
    }

    public boolean equals(Object o){
        if(this == o) return true;

        if(o == null || this.getClass() != o.getClass() ) return false;

        UserDraft u = (UserDraft) o;

        return (u.getEmail() == this.email
            && u.getPassword() == this.password
            && u.getName() == this.name
            && u.getGender() == this.gender
            && u.getHeight() == this.height
            && u.getWeight() == this.weight
            && (u.birthDate).equals(this.birthDate)
            && u.getFavoriteSport() == this.favoriteSport);
    }


}