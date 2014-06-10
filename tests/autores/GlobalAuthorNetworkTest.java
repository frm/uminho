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
	}

	@Test
	public void testTopPublishers() {
		fail("Not yet implemented");
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
	public void testAuthorsInIntervalTupleOfIntegerInteger() {
		fail("Not yet implemented");
	}

	@Test
	public void testAuthorsInIntervalIntInt() {
		fail("Not yet implemented");
	}

	@Test
	public void testGetCoauthorsOf() {
		fail("Not yet implemented");
	}

}
