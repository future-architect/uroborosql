package jp.co.future.uroborosql.sample;

import static org.junit.Assert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.junit.BeforeClass;
import org.junit.Test;

/**
 * SqlAgentSampleクラスのテストケース
 *
 * （注意）
 * このテストケースをテストする際は、src/test/resources をクラスパスに含めてください。（sqlファイルが参照できません）
 *
 * @author H.Sugimoto
 * @version 2014/09/05 新規作成
 */
public class SqlAgentSampleTest {
	private static SqlAgentSampleApp app = null;

	private static Map<String, Object> row1 = new LinkedHashMap<>();
	private static Map<String, Object> row2 = new LinkedHashMap<>();
	private static Map<String, Object> row3 = new LinkedHashMap<>();

	@SuppressWarnings("deprecation")
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:SqlAgentSampleTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE )");

				try (PreparedStatement pstmt = conn.prepareStatement("insert into test values (?, ?, ?, ?)")) {
					pstmt.setInt(1, 1);
					pstmt.setString(2, "aaa");
					pstmt.setInt(3, 10);
					pstmt.setDate(4, new java.sql.Date(100, 0, 1));
					pstmt.addBatch();

					pstmt.setInt(1, 2);
					pstmt.setString(2, "あああ");
					pstmt.setInt(3, 20);
					pstmt.setDate(4, new java.sql.Date(100, 1, 1));
					pstmt.addBatch();

					pstmt.setInt(1, 3);
					pstmt.setString(2, "1111");
					pstmt.setInt(3, 3000);
					pstmt.setDate(4, new java.sql.Date(100, 2, 1));
					pstmt.addBatch();

					pstmt.executeBatch();

					conn.commit();
				}
			}
		}

		row1.put("ID", 1);
		row1.put("NAME", "aaa");
		row1.put("AGE", 10);
		row1.put("BIRTHDAY", new java.sql.Date(100, 0, 1));

		row2.put("ID", 2);
		row2.put("NAME", "あああ");
		row2.put("AGE", 20);
		row2.put("BIRTHDAY", new java.sql.Date(100, 1, 1));

		row3.put("ID", 3);
		row3.put("NAME", "1111");
		row3.put("AGE", 3000);
		row3.put("BIRTHDAY", new java.sql.Date(100, 2, 1));

		app = new SqlAgentSampleApp(url, user, password);
	}

	@Test
	public void sqlQueryTestNoParam() throws Exception {
		List<Map<String, Object>> actual = app.query("example/select_test");

		List<Map<String, Object>> expected = new ArrayList<>();
		expected.add(row1);
		expected.add(row2);
		expected.add(row3);

		assertEquals(toString(expected), toString(actual));
	}

	@Test
	public void sqlQueryTestWithId() throws Exception {
		Map<String, Object> params = new HashMap<>();
		params.put("id", 1);
		List<Map<String, Object>> actual = app.query("example/select_test", params);

		List<Map<String, Object>> expected = new ArrayList<>();
		expected.add(row1);

		assertEquals(toString(expected), toString(actual));
	}

	@Test
	public void sqlQueryTestWithName() throws Exception {
		Map<String, Object> params = new HashMap<>();
		params.put("name", "あああ");
		List<Map<String, Object>> actual = app.query("example/select_test", params);

		List<Map<String, Object>> expected = new ArrayList<>();
		expected.add(row2);

		assertEquals(toString(expected), toString(actual));
	}

	private String toString(final List<Map<String, Object>> obj) {
		if (obj == null) {
			return "";
		} else {
			return obj.stream()
					.map(m -> m.entrySet().stream()
							.map(e -> e.getKey() + "=" + Objects.toString(e.getValue()))
							.collect(Collectors.joining(",", "{", "}")))
					.collect(Collectors.joining(",", "[", "]"));
		}
	}

}
