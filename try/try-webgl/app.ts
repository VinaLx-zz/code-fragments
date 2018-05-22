import * as express from "express"

const app  = express();
app.use("/test", express.static("."));
app.listen(8080, "0.0.0.0");