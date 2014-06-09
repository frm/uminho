package autores;

import java.util.TreeMap;

public class GlobalAuthorNetwork {
	private TreeMap<Integer, AuthorCatalog> annualNetworks;
	
	public GlobalAuthorNetwork() {
		this.annualNetworks = new TreeMap<Integer, AuthorCatalog>();
	}
}
