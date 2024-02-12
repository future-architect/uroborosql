package jp.co.future.uroborosql.client.command;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

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
import jp.co.future.uroborosql.client.completer.SqlKeywordCompleter;
import jp.co.future.uroborosql.client.completer.TableNameCompleter;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.utils.StringUtils;

public class GenerateCommandTest extends ReaderTestSupport {
	private SqlConfig sqlConfig;

	private SqlAgent agent;

	private ReplCommand command;

	@Override
	@BeforeEach
	public void setUp() throws Exception {
		super.setUp();

		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1"))
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

		command = new GenerateCommand();
	}

	@AfterEach
	public void tearDown() throws Exception {
		agent.close();
	}

	@Test
	void testExecuteSelect() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		assertThat(command.execute(reader, "generate select PRODUCT".split("\\s+"), sqlConfig, new Properties()),
				is(true));
		assertConsoleOutputContains("SELECT");
	}

	@Test
	void testExecuteInsert() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		assertThat(command.execute(reader, "generate insert PRODUCT".split("\\s+"), sqlConfig, new Properties()),
				is(true));
		assertConsoleOutputContains("INSERT");
	}

	@Test
	void testExecuteInsertWithAutoIncrement() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		assertThat(command.execute(reader, "generate insert GEN_TEST".split("\\s+"), sqlConfig, new Properties()),
				is(true));
		assertThat(trimWhitespace(out.toString()), is(
				"INSERT /* _SQL_ID_ */ INTO GEN_TEST ( /*IF id != null */  , \"ID\" /*END*/ /*IF name != null */  , \"NAME\" /*END*/ /*IF lockNo != null */  , \"LOCK_NO\" /*END*/ ) VALUES ( /*IF id != null */  , /*id*/'' /*END*/ /*IF name != null */  , /*name*/'' /*END*/ /*IF lockNo != null */  , /*lockNo*/'' /*END*/ )"));
	}

	private String trimWhitespace(final String str) {
		return str.trim().replaceAll("\r\n|\r|\n|\t+|\\s+", " ");
	}

	@Test
	void testExecuteUpdate() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		assertThat(command.execute(reader, "generate update PRODUCT".split("\\s+"), sqlConfig, new Properties()),
				is(true));
		assertConsoleOutputContains("UPDATE");
	}

	@Test
	void testExecuteDelete() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		assertThat(command.execute(reader, "generate delete PRODUCT".split("\\s+"), sqlConfig, new Properties()),
				is(true));
		assertConsoleOutputContains("DELETE");
	}

	@Test
	void testExecuteWithVersionColumn() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		var props = new Properties();
		props.put("sql.versionColumnName", "lock_no");
		props.put("sql.optimisticLockSupplier",
				"jp.co.future.uroborosql.mapping.FieldIncrementOptimisticLockSupplier");
		command.execute(reader, "generate update GEN_TEST".split("\\s+"), sqlConfig, props);
		assertThat(trimWhitespace(out.toString()), is(
				"UPDATE /* _SQL_ID_ */ GEN_TEST SET /*IF name != null */  , \"NAME\" = /*name*/'' /*END*/  , \"LOCK_NO\" = /*SF.increment(lockNo)*/ WHERE   \"ID\" = /*id*/''  AND \"LOCK_NO\" = /*lockNo*/''"));
	}

	@Test
	void testExecuteWithoutVersionColumn() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		var props = new Properties();
		command.execute(reader, "generate update GEN_TEST".split("\\s+"), sqlConfig, props);
		assertThat(trimWhitespace(out.toString()), is(
				"UPDATE /* _SQL_ID_ */ GEN_TEST SET /*IF name != null */  , \"NAME\" = /*name*/'' /*END*/ /*IF lockNo != null */  , \"LOCK_NO\" = /*lockNo*/'' /*END*/ WHERE   \"ID\" = /*id*/''"));
	}

	@Test
	void testShowHelp() throws Exception {
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);
		command.showHelp(reader.getTerminal());
		reader.flush();
		assertConsoleOutputContains("generate sql to access the table.");
	}

	@Test
	void testGetStartArgNo() throws Exception {
		assertThat(command.getStartArgNo(ReplCommandCompleter.class), is(-1));
		assertThat(command.getStartArgNo(SqlKeywordCompleter.class), is(1));
		assertThat(command.getStartArgNo(TableNameCompleter.class), is(2));
	}

	@Test
	void testUtilityMethods() throws Exception {
		assertThat(command.isHidden(), is(false));
		assertThat(command.match("Generate"), is(true));
		assertThat(command.match("NoMatchCommand"), is(false));
		assertThat(command.is("generate"), is(true));
		assertThat(command.is("not"), is(false));
	}

}
