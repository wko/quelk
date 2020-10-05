import de.tu_dresden.epistemic_rewriter.web.WebAppTNCQ
import javax.servlet.ServletContext
import org.scalatra._

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context.mount(new WebAppTNCQ, "/*")
    //context.initParameters("org.scalatra.environment") = "production"
  }
}
