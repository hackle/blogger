using System;
using System.Linq;
using System.Collections.Generic;
					
public class Program
{
	public static void Main()
	{
		IEnumerable<int> fibo = Enumerable.Empty<int>();
		var lazyFibo = new Lazy<IEnumerable<int>>(() => fibo);
		fibo = new [] { 0, 1 }.ConcatLazy(lazyFibo).Zip(
			Ext.SkipLazy(lazyFibo, 1),
			(a, b) => a + b);
		
		Console.WriteLine(string.Join(",", fibo.Take(3)));
	}
}

public static class Ext
{
	public static IEnumerable<T> ConcatLazy<T>(this IEnumerable<T> @this, Lazy<IEnumerable<T>> v) 
	{
		foreach (var t in @this) yield return t;
		var xs = v.Value;
		foreach (var t in xs) yield return t;
	}

	public static IEnumerable<T> ZipLazy<T>(this IEnumerable<T> @this, IEnumerable<T> @that, Func<T, T, T> zip) 
	{
		using var enum1 = @this.GetEnumerator();
		using var enum2 = @that.GetEnumerator();

		while (enum1.MoveNext() && enum2.MoveNext()) yield return zip(enum1.Current, enum2.Current);
	}

	public static IEnumerable<T> SkipLazy<T>(Lazy<IEnumerable<T>> @this, int count)
	{
		var skipped = 0;
		var xs = @this.Value;
		foreach (var t in xs)
		{
			if (skipped++ >= count) yield return t;
		}
	}
}