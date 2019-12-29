package jp.co.future.uroborosql.client.command;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.completer.ReplCommandCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.utils.StringUtils;

public class ClsCommandTest extends ReaderTestSupport {
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

		command = new ClsCommand();
	}

	@After
	public void tearDown() throws Exception {
		agent.close();
	}

	@Test
	public void testExecute() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		boolean flag = command.execute(reader, "cls".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
	}

	@Test
	public void testShowHelp() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		command.showHelp(terminal);
		reader.flush();
		assertConsoleOutputContains("clear screen.");
	}

	@Test
	public void testGetStartArgNo() throws Exception {
		assertThat(command.getStartArgNo(ReplCommandCompleter.class), is(-1));
	}

	@Test
	public void testUtilityMethods() throws Exception {
		assertThat(command.isHidden(), is(false));
		assertThat(command.match("Cls"), is(true));
		assertThat(command.match("NoMatchCommand"), is(false));
		assertThat(command.is("cls"), is(true));
		assertThat(command.is("not"), is(false));
	}

}
