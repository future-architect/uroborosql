package jp.co.future.uroborosql.client.completer;

import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

import org.jline.reader.LineReader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.command.ReplCommand;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.store.SqlResourceManagerImpl;

public class BindParamCompleterTest extends ReaderTestSupport {
	private static List<ReplCommand> commands = new ArrayList<>();
	private SqlConfig sqlConfig;

	@BeforeAll
	public static void setUpClass() throws Exception {
		// ReplCommandの読み込み
		for (var command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}
	}

	@Override
	@BeforeEach
	public void setUp() throws Exception {
		super.setUp();
		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1"))
				.setSqlResourceManager(new SqlResourceManagerImpl(false))
				.setDialect(new DefaultDialect())
				.build();
	}

	@Test
	void testComplete() throws Exception {
		var completer = new BindParamCompleter(commands, sqlConfig);
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
		assertBuffer("update example/insert_product product_description=",
				new TestBuffer("update example/insert_product ").tab().tab().tab().tab());
		assertBuffer("update example/insert_product version_no=1",
				new TestBuffer("update example/insert_product version_no=1").tab());
		assertBuffer("update example/insert_product version_no=1 product_description=",
				new TestBuffer("update example/insert_product version_no=1 ").tab().tab().tab().tab());
		assertBuffer("query example/select_product2 ", new TestBuffer("query example/select_product2 ").tab());
		assertBuffer("update example/insert_product product_id='11 1' jan_code=[10, 20] upd_datetime=",
				new TestBuffer("update example/insert_product product_id='11 1' jan_code=[10, 20] u").tab());

	}

}
