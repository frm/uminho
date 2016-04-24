package autores;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

public class AuthorInfoTest {

	private AuthorInfo authorInfo;

	@Before
	public void setUp() throws Exception {
		authorInfo = new AuthorInfo("Tua Prima");
	}

	@Test
	public void testGetSoloPublications() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		int b = authorInfo.getSoloPublications();
		assertEquals(3, b);
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Maybe Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		b = authorInfo.getSoloPublications();
		assertEquals(4, b);
	}
	
	@Test
	public void testOnlySolo() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		boolean b = authorInfo.onlySolo();
		assertEquals(true, b);
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		b = authorInfo.onlySolo();
		assertEquals(false, b);
	}
	
	@Test
	public void testNeverSolo() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Maybe Tua Prima", "Not Tua Prima", "Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		boolean b = authorInfo.neverSolo();
		assertEquals(true, b);
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		b = authorInfo.neverSolo();
		assertEquals(false, b);
	}
	
	@Test
	public void testTotalPublications() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Maybe Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima"}));
		int b = authorInfo.getTotalPublications();
		assertEquals(3, b);
	}
	
	@Test
	public void testGetCoauthors() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Maybe Tua Prima", "Not Tua Prima"}));
		Set<String> set = authorInfo.getCoauthors();
		int i = set.size();
		assertEquals(2, i);
		boolean b = authorInfo.getCoauthors().contains("Not Tua Prima");
		assertEquals(true, b);
		b = authorInfo.getCoauthors().contains("Maybe Tua Prima");
		assertEquals(true, b);
	}
	
	@Test
	public void testGetCoauthorsInfo() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Maybe Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		Map<String, Integer> map = authorInfo.getCoauthorsInfo();
		int b = map.size();
		assertEquals(3, b);
		b = map.get("Not Tua Prima");
		assertEquals(3, b);
		b = map.get("Maybe Tua Prima");
		assertEquals(1, b);
		b = map.get("Trol");
		assertEquals(2, b);
	}
	
	@Test
	public void testTopCoauthors() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Maybe Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		Set<Tuple<String, Integer>> set;
		set = authorInfo.topCoauthors(2);
		int i = set.size();
		assertEquals(2, i);
		boolean b = set.contains(new Tuple<String, Integer>("Not Tua Prima", 3));
		assertEquals(true, b);
		b = set.contains(new Tuple<String, Integer>("Trol", 2));
		assertEquals(true, b);
	}
	
	@Test
	public void testGetAuthorsPairs() {
		//authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Crol", "Maybe Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Crol", "Brol"}));
		//authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		Map<Tuple<String, String>, Integer> map = authorInfo.getAuthorPairs();
		int i = map.size();
		assertEquals(2, i);
		//i = map.get(new Tuple<String, String>("Maybe Tua Prima", "Tua Prima"));
		//assertEquals(1, i);
		//i = map.get(new Tuple<String, String>("Trol", "Tua Prima"));
		//assertEquals(5, i);
		i = map.get(new Tuple<String, String>("Brol", "Tua Prima"));
		assertEquals(4, i);
		i = map.get(new Tuple<String, String>("Crol", "Tua Prima"));
		assertEquals(1, i);
		
		AuthorInfo info1 = new AuthorInfo("Crol");
		AuthorInfo info2 = new AuthorInfo("Brol");
		info2.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		info2.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		info2.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		info2.addPublication(Arrays.asList(new String[]{"Tua Prima", "Crol", "Brol"}));
		info1.addPublication(Arrays.asList(new String[]{"Tua Prima", "Crol", "Brol"}));
		
		map = info1.getAuthorPairs();
		i = map.size();
		assertEquals(2, i);
		i = map.get(new Tuple<String, String>("Brol", "Crol"));
		assertEquals(1, i);
		
		map = info2.getAuthorPairs();
		i = map.size();
		assertEquals(2, i);
		i = map.get(new Tuple<String, String>("Brol", "Crol"));
		assertEquals(1, i);
		i = map.get(new Tuple<String, String>("Brol", "Tua Prima"));
		assertEquals(4, i);
	}
	
	@Test
	public void testTotalCoauthors() {
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Not Tua Prima", "Tua Prima", "Maybe Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol", "Not Tua Prima"}));
		authorInfo.addPublication(Arrays.asList(new String[]{"Trol", "Tua Prima"}));
		int i = authorInfo.getTotalPublications();
		assertEquals(4, i);
	}
}
