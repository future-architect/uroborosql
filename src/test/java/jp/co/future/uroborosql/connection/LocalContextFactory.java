package jp.co.future.uroborosql.connection;

import java.util.Hashtable;

import javax.naming.Binding;
import javax.naming.Context;
import javax.naming.Name;
import javax.naming.NameClassPair;
import javax.naming.NameParser;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.spi.InitialContextFactory;

/**
 * テスト用のJNDIコンテキストファクトリ
 *
 * @author H.Sugimoto
 */
public class LocalContextFactory implements InitialContextFactory {
	private static final Context CONTEXT = new LocalContext();

	@Override
	public Context getInitialContext(final Hashtable<?, ?> environment) throws NamingException {
		return CONTEXT;
	}

	private static class LocalContext implements Context {
		private final Hashtable<Object, Object> environment = new Hashtable<>();

		private String nameToString(final Name name) {
			var enumeration = name.getAll();
			var builder = new StringBuilder();
			while (enumeration.hasMoreElements()) {
				builder.append(enumeration.nextElement());
			}
			return builder.toString();
		}

		@Override
		public Object lookup(final Name name) throws NamingException {
			return lookup(nameToString(name));
		}

		@Override
		public Object lookup(final String name) throws NamingException {
			if (environment.containsKey(name)) {
				return environment.get(name);
			} else {
				throw new NamingException();
			}
		}

		@Override
		public void bind(final Name name, final Object obj) throws NamingException {
			bind(nameToString(name), obj);
		}

		@Override
		public void bind(final String name, final Object obj) throws NamingException {
			environment.put(name, obj);
		}

		@Override
		public void rebind(final Name name, final Object obj) throws NamingException {
		}

		@Override
		public void rebind(final String name, final Object obj) throws NamingException {
		}

		@Override
		public void unbind(final Name name) throws NamingException {
		}

		@Override
		public void unbind(final String name) throws NamingException {
		}

		@Override
		public void rename(final Name oldName, final Name newName) throws NamingException {
		}

		@Override
		public void rename(final String oldName, final String newName) throws NamingException {
		}

		@Override
		public NamingEnumeration<NameClassPair> list(final Name name) throws NamingException {
			return null;
		}

		@Override
		public NamingEnumeration<NameClassPair> list(final String name) throws NamingException {
			return null;
		}

		@Override
		public NamingEnumeration<Binding> listBindings(final Name name) throws NamingException {
			return null;
		}

		@Override
		public NamingEnumeration<Binding> listBindings(final String name) throws NamingException {
			return null;
		}

		@Override
		public void destroySubcontext(final Name name) throws NamingException {
		}

		@Override
		public void destroySubcontext(final String name) throws NamingException {
		}

		@Override
		public Context createSubcontext(final Name name) throws NamingException {
			return this;
		}

		@Override
		public Context createSubcontext(final String name) throws NamingException {
			return this;
		}

		@Override
		public Object lookupLink(final Name name) throws NamingException {
			return null;
		}

		@Override
		public Object lookupLink(final String name) throws NamingException {
			return null;
		}

		@Override
		public NameParser getNameParser(final Name name) throws NamingException {
			return null;
		}

		@Override
		public NameParser getNameParser(final String name) throws NamingException {
			return null;
		}

		@Override
		public Name composeName(final Name name, final Name prefix) throws NamingException {
			return null;
		}

		@Override
		public String composeName(final String name, final String prefix) throws NamingException {
			return null;
		}

		@Override
		public Object addToEnvironment(final String propName, final Object propVal) throws NamingException {
			return null;
		}

		@Override
		public Object removeFromEnvironment(final String propName) throws NamingException {
			return null;
		}

		@Override
		public Hashtable<?, ?> getEnvironment() throws NamingException {
			return environment;
		}

		@Override
		public void close() throws NamingException {
		}

		@Override
		public String getNameInNamespace() throws NamingException {
			return null;
		}
	}

}
