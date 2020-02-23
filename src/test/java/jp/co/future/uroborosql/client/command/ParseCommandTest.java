package jp.co.future.uroborosql.client.command;

import static org.junit.Assert.*;

import java.sql.DriverManager;
import java.util.Properties;

import org.jline.reader.LineReader;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.store.NioSqlManagerImpl;

public class ParseCommandTest extends ReaderTestSupport {
	private SqlConfig sqlConfig;

	private ReplCommand command;

	@Override
	@Before
	public void setUp() throws Exception {
		super.setUp();

		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.setSqlManager(new NioSqlManagerImpl())
				.build();

		command = new ParseCommand();
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testExecute() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		boolean flag = command.execute(reader, "parse test/PARSE_TEST".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("PARSE:");
		assertConsoleOutputContains("BRANCHES :");
		assertConsoleOutputContains("\tEmbeddedValue : TABLE_NAME");
		assertConsoleOutputContains("\tBegin {");
		assertConsoleOutputContains("\t\tif ( param11 != null ) {");
		assertConsoleOutputContains("\t\t\tBindVariable : param11");
		assertConsoleOutputContains("\t\t} else if ( param11 == null and param12 != null ) {");
		assertConsoleOutputContains("\t\t\tBindVariable : param12");
		assertConsoleOutputContains("\t\t} else {");
		assertConsoleOutputContains("\t\t\tif ( SF.isNotEmpty(param21) ) {");
		assertConsoleOutputContains("\t\t\t\tParenBindVariable : param21");
		assertConsoleOutputContains(
				"\t\t\t} else if ( SF.isEmpty(param21) and SF.isNotEmpty(param22) or SF.isNotEmpty(param23) ) {");
		assertConsoleOutputContains("\t\t\t\tParenBindVariable : param22");
		assertConsoleOutputContains("\t\t\t}");
		assertConsoleOutputContains("\t\t}");
		assertConsoleOutputContains("\t}");
		assertConsoleOutputContains("BIND_PARAMS :");
		assertConsoleOutputContains("\tTABLE_NAME");
		assertConsoleOutputContains("\tparam11");
		assertConsoleOutputContains("\tparam12");
		assertConsoleOutputContains("\tparam21");
		assertConsoleOutputContains("\tparam22");
		assertConsoleOutputContains("\tparam23");
	}

	@Test
	public void testExecuteNotFound() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		boolean flag = command.execute(reader, "parse test/NOTFOUND".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("PARSE:");
		assertConsoleOutputContains("sqlName : test/NOTFOUND not found.");
	}

	@Test
	public void testExecuteNotArgument() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		boolean flag = command.execute(reader, "parse".split("\\s+"), sqlConfig, new Properties());
		assertTrue(flag);
		assertConsoleOutputContains("PARSE:");
		assertConsoleOutputContains("sqlName must be specified.");
	}

	@Test
	public void testShowHelp() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		command.showHelp(reader.getTerminal());
		reader.flush();
		assertConsoleOutputContains("parse sql file.");
		assertConsoleOutputContains("ex) parse [sql file name]<Enter> : Parse sql file.");
	}

}
