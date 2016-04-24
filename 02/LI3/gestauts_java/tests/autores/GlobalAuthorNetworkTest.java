package autores;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

public class GlobalAuthorNetworkTest {

	private GlobalAuthorNetwork globalAuthorNetwork;
	
	@Before
	public void setUp() throws Exception {
		globalAuthorNetwork = new GlobalAuthorNetwork();
		globalAuthorNetwork.addPublication(1991, Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		globalAuthorNetwork.addPublication(1992, Arrays.asList(new String[]{"Tua Prima", "Crol", "Maybe Tua Prima"}));
		globalAuthorNetwork.addPublication(1993, Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		globalAuthorNetwork.addPublication(1993, Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		globalAuthorNetwork.addPublication(1993, Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		globalAuthorNetwork.addPublication(1993, Arrays.asList(new String[]{"Tua Prima", "Crol", "Brol"}));
		globalAuthorNetwork.addPublication(1994, Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		globalAuthorNetwork.addPublication(1995, Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		globalAuthorNetwork.addPublication(1996, Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		globalAuthorNetwork.addPublication(1997, Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		globalAuthorNetwork.addPublication(1998, Arrays.asList(new String[]{"Solo"}));
		globalAuthorNetwork.addPublication(1999, Arrays.asList(new String[]{"Another Solo"}));
		globalAuthorNetwork.addPublication(1999, Arrays.asList(new String[]{"Solo"}));
		globalAuthorNetwork.addPublication(1999, Arrays.asList(new String[]{"Trol"}));
	}

	@Test
	public void testTopPublishers() {
		Set<Tuple<String, Integer>> set = globalAuthorNetwork.topPublishers(1991, 1997, 4);
		int i = set.size();
		assertEquals(4, i);
		boolean b = set.contains(new Tuple<String, Integer>("Tua Prima", 10));
		assertEquals(true, b);
		b = set.contains(new Tuple<String, Integer>("Brol", 4));
		assertEquals(true, b);
		b = set.contains(new Tuple<String, Integer>("Trol", 5));
		assertEquals(true, b);
		b = set.contains(new Tuple<String, Integer>("Crol", 2));
		assertEquals(true, b);
	}

	@Test
	public void testTopPairs() {
		Set<Tuple<Tuple<String, String>, Integer>> set = globalAuthorNetwork.topPairs(1991, 1997, 2);
		for (Tuple<Tuple<String, String>, Integer> t : set) {
			System.out.println(t.getFirst().getFirst() + " " + t.getFirst().getSecond() + " " + t.getSecond());
		}
		int i = set.size();
		assertEquals(2, i);
		boolean b = set.contains(new Tuple<Tuple<String, String>, Integer>(new Tuple<String, String>("Trol", "Tua Prima"), 5));
		assertEquals(true, b);
		b = set.contains(new Tuple<Tuple<String, String>, Integer>(new Tuple<String, String>("Brol", "Tua Prima"), 4));
		assertEquals(true, b);
	}

	@Test
	public void testAuthorsInInterval() {
		try {
			Set<String> set = globalAuthorNetwork.authorsInInterval(1992, 1993);
			
			int i = set.size();
			assertEquals(2, i);
			boolean b = set.containsAll(Arrays.asList(new String[]{"Tua Prima", "Crol"}));
			assertEquals(true, b);
		}
		catch (NoAuthorsInIntervalException e) {
			
		}
	}

	@Test
	public void testGetCoauthorsOf() {
		Set<String> set = globalAuthorNetwork.getCoauthorsOf("Crol");
		
		int i = set.size();
		assertEquals(3, i);
		boolean b = set.containsAll(Arrays.asList(new String[]{"Tua Prima", "Brol", "Maybe Tua Prima"}));
		assertEquals(true, b);
	}
	
	@Test
	public void testGetSoloAuthors() {
		Set<String> set = globalAuthorNetwork.getSoloAuthors();
		
		int i = set.size();
		assertEquals(2, i);
		boolean b = set.containsAll(Arrays.asList(new String[]{"Solo", "Another Solo"}));
		assertEquals(true, b);
	}
	
	@Test
	public void testGetNonSoloAuthors() {
		Set<String> set = globalAuthorNetwork.getNonSoloAuthors();
		
		int i = set.size();
		assertEquals(4, i);
		boolean b = set.containsAll(Arrays.asList(new String[]{"Tua Prima", "Crol", "Maybe Tua Prima", "Brol"}));
		assertEquals(true, b);
	}
}
