package jp.co.future.uroborosql.client.command;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.List;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.completer.BindParamCompleter;
import jp.co.future.uroborosql.client.completer.ReplCommandCompleter;
import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.store.SqlResourceManagerImpl;
import jp.co.future.uroborosql.utils.ObjectUtils;

public class UpdateCommandTest extends ReaderTestSupport {
	private SqlConfig sqlConfig;

	private SqlAgent agent;

	private ReplCommand command;

	@Override
	@BeforeEach
	public void setUp() throws Exception {
		super.setUp();

		sqlConfig = UroboroSQL
				.builder(DriverManager
						.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1"))
				.setSqlResourceManager(new SqlResourceManagerImpl())
				.build();
		agent = sqlConfig.agent();

		var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (var sql : sqls) {
			if (ObjectUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
		agent.commit();
		command = new UpdateCommand();
		List.of("ddl/create_tables",
				"example/insert_column_type_test",
				"example/insert_product",
				"example/insert_product_for_bean",
				"example/insert_product_for_optional",
				"example/insert_product_regist_work",
				"example/select_column_type_test",
				"example/select_consts",
				"example/select_enum",
				"example/select_product",
				"example/select_product_param_camel",
				"example/select_product_where_upd_datetime",
				"example/select_test",
				"example/selectinsert_product",
				"example/selectinsert_product2")
				.forEach(sqlName -> sqlConfig.getSqlResourceManager().getSql(sqlName));
	}

	@AfterEach
	public void tearDown() throws Exception {
		agent.close();
	}

	@Test
	void testExecute() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		var flag = command.execute(reader,
				"update example/insert_product product_id=111 product_name=name product_kana_name=kana product_description=desc jan_code=111111 ins_datetime='2019-01-01T01:01:01' upd_datetime='2019-01-01T01:01:01' version_no=1"
						.split("\\s+"),
				sqlConfig,
				new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("update sql[example/insert_product] end. row count=1");

		command.execute(reader, "update example/insert_product product_id=111".split("\\s+"), sqlConfig,
				new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("Error : ");

		command.execute(reader, "update example/insert_product_not_found".split("\\s+"), sqlConfig,
				new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("SQL not found. sql=example/insert_product_not_found");
	}

	@Test
	void testShowHelp() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		command.showHelp(reader.getTerminal());
		reader.flush();
		assertConsoleOutputContains("execute update from loaded sql file.");
	}

	@Test
	void testGetStartArgNo() throws Exception {
		assertThat(command.getStartArgNo(ReplCommandCompleter.class), is(-1));
		assertThat(command.getStartArgNo(SqlNameCompleter.class), is(1));
		assertThat(command.getStartArgNo(BindParamCompleter.class), is(2));
	}

	@Test
	void testUtilityMethods() throws Exception {
		assertThat(command.isHidden(), is(false));
		assertThat(command.match("Update"), is(true));
		assertThat(command.match("NoMatchCommand"), is(false));
		assertThat(command.is("update"), is(true));
		assertThat(command.is("not"), is(false));
	}

}
