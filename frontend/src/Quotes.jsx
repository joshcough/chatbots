import React from "react"
import { Table } from "react-bootstrap"

export const View = props =>
  <div>
   <Table striped bordered hover>
     <thead>
       <tr>
         <th>Channel</th>
         <th>Quote Id</th>
         <th>Quote</th>
       </tr>
     </thead>
     <tbody>
        {props.quotes.map((q, _) =>
          <tr>
            <td>{q.channel}</td>
            <td>{q.qid}</td>
            <td>{q.body}</td>
          </tr>
         )}
     </tbody>
   </Table>
  </div>
