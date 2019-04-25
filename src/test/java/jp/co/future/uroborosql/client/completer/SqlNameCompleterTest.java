package jp.co.future.uroborosql.client.completer;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

import org.jline.reader.LineReader;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.store.NioSqlManagerImpl;
import jp.co.future.uroborosql.store.SqlManager;

public class SqlNameCompleterTest extends ReaderTestSupport {
	private static List<ReplCommand> commands = new ArrayList<>();
	private static SqlManager sqlManager;

	@BeforeClass
	public static void setUpClass() throws Exception {
		// ReplCommandの読み込み
		for (ReplCommand command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}

		sqlManager = new NioSqlManagerImpl(false);
		sqlManager.setDialect(new DefaultDialect());
		sqlManager.initialize();
	}

	@Test
	public void testComplete() throws Exception {
		SqlNameCompleter completer = new SqlNameCompleter(commands, sqlManager);
		reader.setCompleter(completer);
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		assertBuffer("qu", new TestBuffer("qu").tab());
		assertBuffer("query ddl/create_tables ", new TestBuffer("query dd").tab());
		assertBuffer("query example/select_product", new TestBuffer("query example/select_p").tab());
		assertBuffer("query example/select_test", new TestBuffer("query example").tab().tab().tab());
		assertBuffer("query example/select_product", new TestBuffer("query example/select_product").tab());
		assertBuffer("query example/select_product ", new TestBuffer("query example/select_product ").tab());
		assertBuffer("query example/select_product p",
				new TestBuffer("query example/select_product p").left().left().back().tab());

	}

}
