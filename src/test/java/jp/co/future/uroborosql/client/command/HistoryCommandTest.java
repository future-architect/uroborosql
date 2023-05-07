package jp.co.future.uroborosql.client.command;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.completer.ReplCommandCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.utils.StringUtils;

public class HistoryCommandTest extends ReaderTestSupport {
	private SqlConfig sqlConfig;

	private SqlAgent agent;

	private ReplCommand command;

	@Override
	@BeforeEach
	public void setUp() throws Exception {
		super.setUp();

		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.build();
		agent = sqlConfig.agent();

		var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (var sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
		agent.commit();
		command = new HistoryCommand();
	}

	@AfterEach
	public void tearDown() throws Exception {
		agent.close();
	}

	@Test
	void testExecute() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		assertBuffer("list", new TestBuffer("list"), true);
		assertBuffer("desc PRODUCT", new TestBuffer("desc PRODUCT"), false);

		var flag = command.execute(reader, "history".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("list");
		assertConsoleOutputContains("desc PRODUCT");

		command.execute(reader, "history li".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
	}

	@Test
	void testShowHelp() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		command.showHelp(reader.getTerminal());
		reader.flush();
		assertConsoleOutputContains("list command history.");
	}

	@Test
	void testGetStartArgNo() throws Exception {
		assertThat(command.getStartArgNo(ReplCommandCompleter.class), is(-1));
	}

	@Test
	void testUtilityMethods() throws Exception {
		assertThat(command.isHidden(), is(false));
		assertThat(command.match("History"), is(true));
		assertThat(command.match("NoMatchCommand"), is(false));
		assertThat(command.is("history"), is(true));
		assertThat(command.is("not"), is(false));
	}

}
