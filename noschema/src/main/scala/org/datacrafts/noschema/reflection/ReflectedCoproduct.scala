package org.datacrafts.noschema.reflection

import scala.reflect.runtime.{universe => ru}
import scala.util.Try

import org.datacrafts.noschema.{Context, NoSchemaCoproduct, Operation}
import org.datacrafts.noschema.operator.CoproductOperator
import org.datacrafts.noschema.reflection.NoSchemaReflector.ReflectedScalaType

object ReflectedCoproduct {

  // deal with wrapping of coproduct members as in Scrooge
  case class Member(
    symbol: ru.Symbol,
    wrapper: Option[ru.Symbol],
    context: Context.CoproductElement[Any]
  ) {
    def matchGenericInput(input: Any): Boolean = {
      wrapper.map(
        _.fullName == input.getClass.getCanonicalName.stripSuffix("$")
      ).getOrElse(
        symbol.fullName == input.getClass.getCanonicalName.stripSuffix("$")
      )
    }

    def unwrapGenericInput(input: Any): Any = {
      wrapper.map(_ => input.asInstanceOf[Product].productElement(0)).getOrElse(input)
    }

    def wrapTypedInput(input: Any): Any = {
      wrapper.map(
        symbol => TypeReflector(symbol.typeSignature).companionApply(input)
      ).getOrElse(input)
    }
  }
}
class ReflectedCoproduct(
  runtimeType: ru.Type,
  members: Seq[ReflectedCoproduct.Member]
) extends NoSchemaCoproduct[Any](members.map(_.context)) {

  override lazy val scalaType = new ReflectedScalaType(runtimeType) {
    override def matchInput(input: Any): Option[Any] =
      if (members.exists(_.matchGenericInput(input))) {
        Some(input)
      } else {
        Option.empty
      }
  }

  lazy val reflector = new TypeReflector(runtimeType)

  override def marshal(typeExtractor: CoproductOperator.TypeValueExtractor,
    operation: Operation[Any]
  ): Any = {
    (for (
      member <- members;
      ReflectedCoproduct.Member(memberSymbol, wrapperSymbol, memberContext) = member;
      matchedValue <- typeExtractor.getTypeValue(memberContext);
      result <- Try{
        logDebug(
          s"${reflector.fullName} found matched " +
            s"subclass ${memberContext} for value ${matchedValue}")
        member.wrapTypedInput(
          operation.dependencyOperation(memberContext).marshal(matchedValue)
        )
      }.toOption
    ) yield {
      result
    }
      ).headOption.getOrElse(
      throw new Exception(s"no value among candidate types (${members}) " +
        s"found from $typeExtractor\n" +
        s"${operation.format()}")
    )

  }

  override def unmarshal(input: Any,
    emptyUnion: CoproductOperator.UnionTypeValueCollector,
    operation: Operation[Any]
  ): CoproductOperator.UnionTypeValueCollector = {
    // based on type reflection or result of unmarshaling
    // primitive types just fo type casting must check the type

    val (matchedMember, result) =
    (for (
      member <- members if member.matchGenericInput(input);
      ReflectedCoproduct.Member(memberSymbol, wrapperSymbol, memberContext) = member
    ) yield {
      logDebug(
        s"${reflector.fullName} unmarshal input[${input.getClass}]=${input} " +
          s"as subclass ${memberContext}")
      (
        memberContext,
        // calling unapply on the wrapper symbol will dispatch multiple levels
        operation.dependencyOperation(memberContext)
          .unmarshal(member.unwrapGenericInput(input))

      )
    }).headOption.getOrElse(
      throw new Exception(
        s"input=${input}[${input.getClass.getCanonicalName}] " +
          s"does not match any member ${members.map(_.symbol.fullName)}")
    )

    emptyUnion.addTypeValue(matchedMember, result)
  }

}