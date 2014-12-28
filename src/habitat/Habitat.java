/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package habitat;

import controllers.*;
import data.UsersRepository;
import java.util.HashMap;
import models.Session;
import models.User;

/**
 *
 * @author frmendes
 */
public class Habitat {
    private Session currentUser;
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        System.out.println(User.passwordHash("tiagoddinis"));
        UsersRepository repo = new UsersRepository("name", "pw", "url");
        repo.findBy(
                new HashMap<String, Object>() {{
                    put("username", "my_un");
                    put("id", 1);
                    put("name", "this");
                }}
        );
    }
    
}
