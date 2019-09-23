package jp.co.future.uroborosql;

import static org.junit.Assert.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Before;

import jp.co.future.uroborosql.config.SqlConfig;

public class AbstractDbTest {

	public static class Product {
		private int productId;
		private String productName;
		private String productKanaName;
		private String janCode;
		private String productDescription;
		private Date insDatetime;
		private Date updDatetime;
		private int versionNo;

		public Product() {
		}

		public int getProductId() {
			return productId;
		}

		public void setProductId(final int productId) {
			this.productId = productId;
		}

		public String getProductName() {
			return productName;
		}

		public void setProductName(final String productName) {
			this.productName = productName;
		}

		public String getProductKanaName() {
			return productKanaName;
		}

		public void setProductKanaName(final String productKanaName) {
			this.productKanaName = productKanaName;
		}

		public String getJanCode() {
			return janCode;
		}

		public void setJanCode(final String janCode) {
			this.janCode = janCode;
		}

		public String getProductDescription() {
			return productDescription;
		}

		public void setProductDescription(final String productDescription) {
			this.productDescription = productDescription;
		}

		public Date getInsDatetime() {
			return insDatetime;
		}

		public void setInsDatetime(final Date insDatetime) {
			this.insDatetime = insDatetime;
		}

		public Date getUpdDatetime() {
			return updDatetime;
		}

		public void setUpdDatetime(final Date updDatetime) {
			this.updDatetime = updDatetime;
		}

		public int getVersionNo() {
			return versionNo;
		}

		public void setVersionNo(final int versionNo) {
			this.versionNo = versionNo;
		}
	}

	public static class BaseProductSearchBean {
		private String productName;

		public String getProductName() {
			return this.productName;
		}

		public void setProductName(final String productName) {
			this.productName = productName;
		}
	}

	public static class ProductSearchBean extends BaseProductSearchBean {
		private List<Integer> productIds;

		public List<Integer> getProductIds() {
			return this.productIds;
		}

		public void setProductIds(final List<Integer> productIds) {
			this.productIds = productIds;
		}
	}

	protected SqlConfig config;
	protected SqlAgent agent;

	public AbstractDbTest() {
		super();
	}

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.build();
		config.getSqlAgentFactory().setFetchSize(1000);
		Path ddlPath = getDdlPath();
		if (ddlPath != null) {
			agent = config.agent();
			String[] sqls = new String(Files.readAllBytes(ddlPath), StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		}
	}

	/**
	 * setup時に実行するDDL文のパスを取得する
	 *
	 * @return DDL文を含むSQLファイルのパス
	 */
	protected Path getDdlPath() {
		return Paths.get("src/test/resources/sql/ddl/create_tables.sql");
	}

	@After
	public void tearDown() throws Exception {
		agent.close();
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
				String[] parts = line.split("\t");
				for (String part : parts) {
					String[] keyValue = part.split(":", 2);
					row.put(keyValue[0].toLowerCase(), StringUtils.isBlank(keyValue[1]) ? null : keyValue[1]);
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
		Arrays.asList(tables).stream().forEach(tbl -> {
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
		List<Map<String, Object>> dataList = getDataFromFile(path);

		dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
				.forEach(tbl -> truncateTable(tbl));

		dataList.forEach(map -> {
			try {
				agent.update(map.get("sql").toString()).paramMap(map).count();
			} catch (Exception ex) {
				ex.printStackTrace();
				fail("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage());
			}
		});
	}

}