package autores;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;

public class AuthorCatalogTest {

	private AuthorCatalog authorCatalog;
	private List<String> names;
	private int n_names;
	
	@Before
	public void setUp() throws Exception {
		authorCatalog = new AuthorCatalog();
		names = Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima",
										   "Maybe Tua Prima", "Trol",
										   "Crol", "Solo", "Brol", 
										   "Another Solo"});
		n_names = names.size();
		authorCatalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Not Tua Prima"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Maybe Tua Prima", "Trol"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol", "Crol"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Brol", "Crol"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Trol", "Tua Prima"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Trol", "Tua Prima"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Solo"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Solo"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Trol"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Trol"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Another Solo"}));
		authorCatalog.addPublication(Arrays.asList(new String[]{"Solo"}));
	}

	@Test
	public void testTopPublishers() {
		Set<Tuple<String, Integer>> set = authorCatalog.topPublishers(3);
		int i = set.size();
		assertEquals(3, i);
		boolean b = set.contains(new Tuple<String, Integer>("Trol", 5));
		assertEquals(true, b);
		b = set.contains(new Tuple<String, Integer>("Tua Prima", 4));
		assertEquals(true, b);
		b = set.contains(new Tuple<String, Integer>("Solo", 3));
		assertEquals(true, b);
	}

	@Test
	public void testGetAuthors() {
		boolean b = authorCatalog.getAuthors().containsAll(names);
		assertEquals(true, b);
		int i = authorCatalog.getAuthors().size();
		assertEquals(8, i);
	}

	@Test
	public void testGetCoauthors() {
		boolean b = authorCatalog.getCoauthors("Tua Prima").containsAll(Arrays.asList(new String[] {
				"Brol", "Crol", "Trol", "Not Tua Prima"
		}));
		assertEquals(true, b);
		int i = authorCatalog.getCoauthors("Tua Prima").size();
		assertEquals(4, i);
	}

	@Test
	public void testHasAuthor() {
		boolean b = authorCatalog.hasAuthor("Tua Prima");
		assertEquals(true, b);
		b = authorCatalog.hasAuthor("Trol");
		assertEquals(true, b);
	}

	@Test
	public void testAuthorByPublications() {
		Map<String, Integer> map = authorCatalog.authorByPublications();
		int i = map.size();
		assertEquals(n_names, i);
		i = map.get("Trol");
		assertEquals(5, i);
		i = map.get("Tua Prima");
		assertEquals(4, i);
		i = map.get("Solo");
		assertEquals(3, i);
	}

	@Test
	public void testAuthorPairs() {
		/*
		Map<Tuple<String, String>, Integer> map = authorCatalog.authorPairs();
		int i = map.size();
		assertEquals(5, i);
		i = map.get(new Tuple<String, String>("Trol", "Tua Prima"));
		assertEquals(2, i);
		i = map.get(new Tuple<String, String>("Maybe Tua Prima", "Trol"));
		assertEquals(1, i);
		*/
		AuthorCatalog catalog = new AuthorCatalog();
		//catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Crol", "Maybe Tua Prima"}));
		catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Brol"}));
		catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Crol", "Brol"}));
		//catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		//catalog.addPublication(Arrays.asList(new String[]{"Tua Prima", "Trol"}));
		Map<Tuple<String, String>, Integer> map = catalog.authorPairs();
		int i = map.size();
		assertEquals(3, i);
		i = map.get(new Tuple<String, String>("Brol", "Tua Prima"));
		assertEquals(4, i);
		i = map.get(new Tuple<String, String>("Brol", "Crol"));
		assertEquals(1, i);
		i = map.get(new Tuple<String, String>("Crol", "Tua Prima"));
		assertEquals(1, i);
		//i = map.get(new Tuple<String, String>("Maybe Tua Prima", "Tua Prima"));
		//assertEquals(1, i);
	}
}
