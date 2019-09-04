import React from "react"
import { Table } from "react-bootstrap"

export const View = props =>
  <div>
   <Table striped bordered hover variant="dark">
     <thead>
       <tr>
         <th>Channel</th>
         <th>Command Name</th>
         <th>Command Body</th>
       </tr>
     </thead>
     <tbody>
        {props.commands.map((c, _) =>
          <tr>
            <td>{c.channel}</td>
            <td>{c.name}</td>
            <td>{c.body}</td>
          </tr>
         )}
     </tbody>
   </Table>
  </div>
