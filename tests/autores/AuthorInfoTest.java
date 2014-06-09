package autores;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

public class AuthorInfoTest {

	private AuthorInfo authorInfo;

	@Before
	public void setUp() throws Exception {
		authorInfo = new AuthorInfo("Tua Prima");
		authorInfo.addPublication(Arrays.asList(new String[]{"TROL"}));
	}

	@Test
	public void testGetSoloPublications() {
		boolean b = authorInfo.onlySolo();
		assertEquals(true, b);
	}
}
