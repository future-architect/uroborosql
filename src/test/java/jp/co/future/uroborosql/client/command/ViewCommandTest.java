package jp.co.future.uroborosql.client.command;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.Properties;

import org.apache.commons.lang3.StringUtils;
import org.jline.reader.LineReader;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.completer.ReplCommandCompleter;
import jp.co.future.uroborosql.client.completer.SqlNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;

public class ViewCommandTest extends ReaderTestSupport {
	private SqlConfig sqlConfig;

	private SqlAgent agent;

	private ReplCommand command;

	@Override
	@Before
	public void setUp() throws Exception {
		super.setUp();

		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.build();
		agent = sqlConfig.agent();

		String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
		agent.commit();
		command = new ViewCommand();
	}

	@After
	public void tearDown() throws Exception {
		agent.close();
	}

	@Test
	public void testExecute() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		boolean flag = command.execute(reader, "view example/select_product".split("\\s+"), sqlConfig,
				new Properties());
		assertConsoleOutputContains("SELECT /* _SQL_ID_ */\r\n" +
				"	*\r\n" +
				"FROM\r\n" +
				"	PRODUCT\r\n" +
				"WHERE 1 = 1\r\n" +
				"/*IF product_id != null */\r\n" +
				"AND	PRODUCT_ID	IN	/*product_id*/(0, 2)\r\n" +
				"/*END*/\r\n" +
				"ORDER BY PRODUCT_ID\r\n" +
				"");
		assertTrue(flag);

		command.execute(reader, "view example/select_notfound".split("\\s+"), sqlConfig, new Properties());
		assertConsoleOutputContains("SQL not found. sql=example/select_notfound");
	}

	@Test
	public void testShowHelp() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		command.showHelp(reader.getTerminal());
		reader.flush();
		assertConsoleOutputContains("view sql file.");
	}

	@Test
	public void testGetStartArgNo() throws Exception {
		assertThat(command.getStartArgNo(ReplCommandCompleter.class), is(-1));
		assertThat(command.getStartArgNo(SqlNameCompleter.class), is(1));
	}

	@Test
	public void testUtilityMethods() throws Exception {
		assertThat(command.isHidden(), is(false));
		assertThat(command.match("View"), is(true));
		assertThat(command.match("NoMatchCommand"), is(false));
		assertThat(command.is("view"), is(true));
		assertThat(command.is("not"), is(false));
	}

}
