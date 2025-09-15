package jp.co.future.uroborosql.matrix;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.utils.ObjectUtils;

@Tag("matrix")
public class AbstractMatrixTest {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(AbstractMatrixTest.class);

	private static final Properties jdbcProps;

	protected SqlConfig config;
	protected SqlAgent agent;

	static {
		jdbcProps = new Properties();
		try {
			jdbcProps.load(Thread.currentThread()
					.getContextClassLoader()
					.getResourceAsStream("jdbc.properties"));
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

	public AbstractMatrixTest() {
	}

	@BeforeEach
	public void setUp() throws Exception {
		var testDbType = getTestDbType();
		var url = jdbcProps.getProperty(String.format("%s.url", testDbType));
		log.atInfo()
				.setMessage("Matrix Test DB Type:{}. url:{}")
				.addArgument(testDbType)
				.addArgument(url)
				.log();

		config = UroboroSQL.builder(url, null, null).build();
		config.getSqlAgentProvider().setFetchSize(1000);
		agent = config.agent();
	}

	@AfterEach
	public void tearDown() throws Exception {
		if (agent != null) {
			agent.close();
		}
	}

	/**
	 * テスト対象のDBTypeを取得.
	 *
	 * @return DBTypeを表す文字列 （システムプロパティ：test.db.type に指定された文字列）
	 */
	protected String getTestDbType() {
		return System.getProperty("test.db.type", "h2");
	}

	/**
	 * パスで指定したLTSVファイルから登録用のデータを生成する。
	 * @param path LTSVファイルパス
	 * @return LTSVをMapのListに変換した結果
	 */
	protected List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				var parts = line.split("\t");
				for (var part : parts) {
					var keyValue = part.split(":", 2);
					row.put(keyValue[0].toLowerCase(), ObjectUtils.isBlank(keyValue[1]) ? null : keyValue[1]);
				}
				ans.add(row);
			});
		} catch (IOException e) {
			e.printStackTrace();
		}
		return ans;
	}

	/**
	 * 指定したテーブルのtruncate文を発行する
	 *
	 * @param tables truncateするテーブル名（複数指定可）
	 */
	protected void truncateTable(final Object... tables) {
		List.of(tables).stream().forEach(tbl -> {
			try {
				agent.updateWith("truncate table " + tbl.toString()).count();
			} catch (Exception ex) {
				ex.printStackTrace();
				fail("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage());
			}
		});
	}

	/**
	 * 指定したLTSVファイルのデータをtruncate-insertする
	 *
	 * @param path LTSVファイル
	 */
	protected void cleanInsert(final Path path) {
		var dataList = getDataFromFile(path);

		dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
				.forEach(this::truncateTable);

		dataList.forEach(map -> {
			try {
				agent.update(map.get("sql").toString()).paramMap(map).count();
			} catch (Exception ex) {
				ex.printStackTrace();
				fail("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage());
			}
		});
		agent.commit();
	}

	protected void assertFile(final String expectedFilePath, final String actualFilePath) throws IOException {
		var expected = new String(Files.readAllBytes(Paths.get(expectedFilePath)), StandardCharsets.UTF_8);
		var actual = new String(Files.readAllBytes(Paths.get(actualFilePath)), StandardCharsets.UTF_8);

		assertThat(actual, is(expected));
	}

}