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

public class BindParamCompleterTest extends ReaderTestSupport {
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
		BindParamCompleter completer = new BindParamCompleter(commands, sqlManager);
		reader.setCompleter(completer);
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		assertBuffer("qu", new TestBuffer("qu").tab());
		assertBuffer("query dd", new TestBuffer("query dd").tab());
		assertBuffer("query example/select_product product_id=", new TestBuffer("query example/select_product ").tab());
		assertBuffer("query example/select_product product_id=",
				new TestBuffer("query example/select_product p").tab());
		assertBuffer("query example/select_produc p",
				new TestBuffer("query example/select_product p").left().left().back().tab());
		assertBuffer("query example/select_product x",
				new TestBuffer("query example/select_product x").tab());
		assertBuffer("update example/insert_product version_no=",
				new TestBuffer("update example/insert_product ").tab().tab().tab().tab());
		assertBuffer("update example/insert_product version_no=1",
				new TestBuffer("update example/insert_product version_no=1").tab());
		assertBuffer("update example/insert_product version_no=1 ins_datetime=",
				new TestBuffer("update example/insert_product version_no=1 ").tab().tab().tab().tab());
		assertBuffer("query example/select_product2 ", new TestBuffer("query example/select_product2 ").tab());
		assertBuffer("update example/insert_product product_id='11 1' jan_code=[10, 20] upd_datetime=",
				new TestBuffer("update example/insert_product product_id='11 1' jan_code=[10, 20] u").tab());

	}

}
